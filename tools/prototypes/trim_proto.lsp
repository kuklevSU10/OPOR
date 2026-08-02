;;; ============================================================================
;;;  trim_proto.lsp  —  Фаза 0: прототип обрезки сетки БЕЗ Express Tools
;;;  Гипотеза: обрезку линий по контуру можно сделать чистым LISP через
;;;  vla-IntersectWith + point-in-polygon (ray casting). Никаких acet-/geomcal.
;;;
;;;  Команды:
;;;    TRIMPROTO — самотесты T1..T7, печатает PASS/FAIL (строит и удаляет свою
;;;                временную геометрию, чужие объекты не трогает)
;;;    TRIMSEL   — обрезка на выделении (T8): выбрать контур -> проёмы -> линии
;;;    SUMLEN    — сумма длин линий на слое (замер эталона от старого плагина)
;;;
;;;  Загрузка: APPLOAD -> выбрать этот файл  (или  (load "trim_proto.lsp") )
;;; ============================================================================

(vl-load-com)

;; модельное пространство
(defun *ms* ()
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

;; список doubles -> variant safearray (для AddLightWeightPolyline)
(defun vd (lst / sa)
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length lst)))))
  (vlax-safearray-fill sa lst)
  (vlax-make-variant sa))

;; построить полилинию из списка 2D-точек ((x y)(x y)...)
(defun mk-pline (pts closed / flat o)
  (setq flat '())
  (foreach p pts
    (setq flat (append flat (list (float (car p)) (float (cadr p))))))
  (setq o (vla-AddLightWeightPolyline (*ms*) (vd flat)))
  (if closed (vla-put-Closed o :vlax-true))
  o)

;; построить линию по двум 2D-точкам
(defun mk-line (a b)
  (vla-AddLine (*ms*)
    (vlax-3d-point (float (car a)) (float (cadr a)) 0.0)
    (vlax-3d-point (float (car b)) (float (cadr b)) 0.0)))

;; safearray -> список точек (3-списков)
(defun safearray->points (a / lst res)
  (setq res '())
  (if (and (= (type a) 'safearray)
           (>= (vlax-safearray-get-u-bound a 1)
               (vlax-safearray-get-l-bound a 1)))
    (progn
      (setq lst (vlax-safearray->list a))
      (while (and lst (cddr lst))
        (setq res (cons (list (car lst) (cadr lst) (caddr lst)) res))
        (setq lst (cdddr lst)))))
  (reverse res))

;; все точки пересечения двух vla-объектов (с защитой от сбоев)
(defun obj-ints (o1 o2 / r)
  (or acExtendNone (setq acExtendNone 0))
  (setq r (vl-catch-all-apply
            '(lambda ()
               (safearray->points
                 (vlax-variant-value (vla-IntersectWith o1 o2 acExtendNone))))))
  (if (vl-catch-all-error-p r) '() r))

;; параметр t точки p на отрезке a->b (2D)
(defun param-t (a b p / dx dy len2)
  (setq dx (- (car b) (car a))
        dy (- (cadr b) (cadr a))
        len2 (+ (* dx dx) (* dy dy)))
  (if (equal len2 0.0 1e-12)
    0.0
    (/ (+ (* (- (car p) (car a)) dx)
          (* (- (cadr p) (cadr a)) dy))
       len2)))

;; точка на отрезке a->b по параметру t
(defun pt-at (a b tt)
  (list (+ (car a) (* tt (- (car b) (car a))))
        (+ (cadr a) (* tt (- (cadr b) (cadr a))))
        0.0))

;; M внутри замкнутой полилинии bnd ? (ray casting через IntersectWith)
(defun inside-p (m bnd / far ray n)
  (setq far (list (+ (car m) 1.0e7) (+ (cadr m) 3.71e6) 0.0)) ; косой луч
  (setq ray (vla-AddLine (*ms*) (vlax-3d-point m) (vlax-3d-point far)))
  (setq n (length (obj-ints ray bnd)))
  (vla-Delete ray)
  (= 1 (rem n 2)))                      ; нечётное число пересечений -> внутри

;; сортировка чисел + удаление близких дублей
(defun sort-uniq (lst / s res)
  (setq s (vl-sort lst '<) res '())
  (foreach x s
    (if (or (null res) (> (abs (- x (car res))) 1e-7))
      (setq res (cons x res))))
  (reverse res))

;; ---- ЯДРО: обрезать линию ln по контуру bnd и списку проёмов holes ----
;; возвращает список созданных отрезков (vla-объектов)
(defun clip-line (ln bnd holes / a b ipts ts lay made t0 t1 m)
  (setq a   (vlax-curve-getStartPoint ln)
        b   (vlax-curve-getEndPoint   ln)
        lay (vla-get-Layer ln)
        ipts (obj-ints ln bnd)
        made '())
  (foreach h holes (setq ipts (append ipts (obj-ints ln h))))
  (setq ts (sort-uniq
             (cons 0.0 (cons 1.0
               (mapcar '(lambda (p) (param-t a b p)) ipts)))))
  (while (cdr ts)
    (setq t0 (car ts) t1 (cadr ts)
          m  (pt-at a b (/ (+ t0 t1) 2.0)))
    (if (and (inside-p m bnd)
             (not (vl-some '(lambda (h) (inside-p m h)) holes)))
      (setq made
        (cons (vla-put-Layer*
                (vla-AddLine (*ms*) (vlax-3d-point (pt-at a b t0))
                                    (vlax-3d-point (pt-at a b t1)))
                lay)
              made)))
    (setq ts (cdr ts)))
  (vla-Delete ln)
  made)

;; put-Layer и вернуть сам объект (vla-put-Layer возвращает nil)
(defun vla-put-Layer* (obj lay) (vla-put-Layer obj lay) obj)

;; ============================================================================
;;  САМОТЕСТЫ T1..T7
;; ============================================================================

(defun mk-square ()  (mk-pline '((0 0)(100 0)(100 100)(0 100)) t))
(defun mk-concave () (mk-pline '((0 0)(100 0)(100 100)(60 100)(60 40)(40 40)(40 100)(0 100)) t))
(defun mk-hole ()    (mk-pline '((40 40)(60 40)(60 60)(40 60)) t))
(defun mk-arc ( / o) (setq o (mk-square)) (vla-SetBulge o 1 1.0) o) ; правая сторона -> дуга наружу (bulge>0 = CCW = апекс на +x)

(defun run-test (name bnd holes line exp-len / made tot ok)
  (setq made (clip-line line bnd holes)
        tot  (apply '+ (cons 0.0 (mapcar 'vla-get-Length made)))
        ok   (equal tot exp-len 0.5))
  (princ (strcat "\n " (if ok "[PASS] " "[FAIL] ") name
                 "  ожид=" (rtos exp-len 2 2)
                 "  факт=" (rtos tot 2 2)
                 "  отрезков=" (itoa (length made))))
  (foreach o made (vla-Delete o))      ; очистка
  (vla-Delete bnd)
  (foreach h holes (vla-Delete h))
  ok)

(defun c:trimproto ( / np nt h5)
  (princ "\n=== Ф0: самотесты обрезки (строю/удаляю временную геометрию) ===")
  (setq np 0 nt 0)
  (defun +t (res) (setq nt (1+ nt)) (if res (setq np (1+ np))))

  (+t (run-test "T1 линия целиком внутри"
        (mk-square) '() (mk-line '(20 50) '(80 50)) 60.0))
  (+t (run-test "T2 линия целиком снаружи"
        (mk-square) '() (mk-line '(120 50) '(180 50)) 0.0))
  (+t (run-test "T3 пересекает один раз"
        (mk-square) '() (mk-line '(50 50) '(150 50)) 50.0))
  (+t (run-test "T4 вогнутый контур (2 отрезка)"
        (mk-concave) '() (mk-line '(-10 70) '(110 70)) 80.0))
  (setq h5 (mk-hole))
  (+t (run-test "T5 контур с проёмом (2 отрезка)"
        (mk-square) (list h5) (mk-line '(-10 50) '(110 50)) 80.0))
  (+t (run-test "T6 касание вершины (0 отрезков)"
        (mk-square) '() (mk-line '(50 150) '(150 50)) 0.0))
  (+t (run-test "T7 дуга/скругление контура"
        (mk-arc) '() (mk-line '(50 50) '(200 50)) 100.0))

  (princ (strcat "\n\n=== ИТОГО: " (itoa np) "/" (itoa nt) " PASS ==="))
  (if (= np nt)
    (princ "\nГипотеза подтверждена — обрезка без Express Tools работает.")
    (princ "\nЕсть FAIL — пришли мне какие именно (особенно T6/T7) и текст ошибок."))
  (princ))

;; ============================================================================
;;  T8 — обрезка на реальном выделении
;; ============================================================================

(defun c:trimsel ( / e bnd holes ss i lines)
  (setq holes '() lines '())
  (princ "\nУкажи КОНТУР (замкнутую полилинию):")
  (if (setq e (entsel "\n> "))
    (progn
      (setq bnd (vlax-ename->vla-object (car e)))
      (princ "\nВыбери ПРОЁМЫ (рамкой) или Enter если нет:")
      (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
        (repeat (setq i (sslength ss))
          (setq i (1- i)
                holes (cons (vlax-ename->vla-object (ssname ss i)) holes))))
      (princ "\nВыбери ЛИНИИ для обрезки:")
      (if (setq ss (ssget '((0 . "LINE"))))
        (progn
          (repeat (setq i (sslength ss))
            (setq i (1- i)
                  lines (cons (vlax-ename->vla-object (ssname ss i)) lines)))
          (foreach ln lines (clip-line ln bnd holes))
          (princ (strcat "\nОбрезано линий: " (itoa (length lines))))))))
  (princ))

;; ============================================================================
;;  Замер эталона: суммарная длина линий на слое
;; ============================================================================

(defun c:sumlen ( / lay ss i tot obj q)
  (setq lay (getstring "\nИмя слоя [Enter='сеткаvb']: "))
  (if (= lay "") (setq lay "сеткаvb"))
  (setq ss (ssget "_X" (list (cons 0 "LINE") (cons 8 lay)))
        tot 0.0 i 0 q 0)
  (if ss
    (progn
      (setq q (sslength ss))
      (repeat q
        (setq obj (vlax-ename->vla-object (ssname ss i))
              tot (+ tot (vla-get-Length obj))
              i   (1+ i)))))
  (princ (strcat "\nСлой '" lay "': линий=" (itoa q)
                 "  суммарная длина=" (rtos tot 2 3)))
  (princ))

(princ "\ntrim_proto.lsp загружен. Команды: TRIMPROTO  TRIMSEL  SUMLEN")
(princ)
