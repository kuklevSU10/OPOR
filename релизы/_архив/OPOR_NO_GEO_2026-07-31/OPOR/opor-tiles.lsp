;;; OPOR S4: плитка - порт copXLines(плитки) / del_plitk / trimplitk /
;;; trimPlitkOpp / calc_plitk. Резка Boolean-регионами через COM, как VBA.
;;; Осознанные отклонения от VBA (см. спеку 2026-07-07-s4-tiles-design):
;;; 1) нет регионов-двойников у плиток возле проёма, не задетых проёмом (баг VBA);
;;; 2) QC/QR считаются по объектам сессии, а не селекцией по слою плиткаvb.

;; замкнутая плитка-прямоугольник: origin + стороны vec*sx и perp*sy
(defun opor-tile-polyline (origin vec perp sx sy / c0 c1 c2 c3 coords arr pline)
  (setq c0 origin)
  (setq c1 (opor-v+ origin (opor-v* vec sx)))
  (setq c2 (opor-v+ c1 (opor-v* perp sy)))
  (setq c3 (opor-v+ origin (opor-v* perp sy)))
  (setq coords
    (list (car c0) (cadr c0) (car c1) (cadr c1)
          (car c2) (cadr c2) (car c3) (cadr c3)))
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 7)))
  (vlax-safearray-fill arr coords)
  (setq pline (vla-AddLightWeightPolyline (opor-ms) arr))
  (vla-put-Closed pline :vlax-true)
  (vla-put-Layer pline *opor-layer-tiles*)
  (opor-register-created pline "tile")
  pline)

;; регион из замкнутой кривой; nil при неудаче. Временный - НЕ регистрируется
;; (финальные регионы-плитки регистрируются в момент, когда становятся плиткой)
(defun opor-tile-region (curve / res)
  (setq res (vl-catch-all-apply 'vlax-invoke (list (opor-ms) 'AddRegion (list curve))))
  (if (or (vl-catch-all-error-p res) (not res))
    nil
    (car res)))

(defun opor-tile-area (obj / a)
  (setq a (vl-catch-all-apply 'vla-get-Area (list obj)))
  (if (vl-catch-all-error-p a) 0.0 a))

;; vla-Boolean: 0=Union 1=Intersection 2=Subtraction; other-регион поглощается
(defun opor-tile-boolean (reg op other / res)
  (setq res (vl-catch-all-apply 'vla-Boolean (list reg op other)))
  (not (vl-catch-all-error-p res)))

(defun opor-tile-delete-quiet (obj)
  (vl-catch-all-apply 'vla-Delete (list obj)))

;; центр плитки = середина bbox (для повёрнутого прямоугольника точен)
(defun opor-tile-center (obj / bb)
  (setq bb (opor-bbox obj))
  (list
    (/ (+ (car (car bb)) (car (cadr bb))) 2.0)
    (/ (+ (cadr (car bb)) (cadr (cadr bb))) 2.0)
    0.0))

(defun opor-bbox-overlap-p (b1 b2)
  (and
    (<= (car (car b1)) (car (cadr b2)))
    (<= (car (car b2)) (car (cadr b1)))
    (<= (cadr (car b1)) (cadr (cadr b2)))
    (<= (cadr (car b2)) (cadr (cadr b1)))))

;; a_main: pl.Elevation = 0 на время геометрических операций
(defun opor-tiles-zero-elevation (objs / saved e)
  (setq saved '())
  (foreach obj objs
    (setq e (vl-catch-all-apply 'vla-get-Elevation (list obj)))
    (if (and (not (vl-catch-all-error-p e)) (numberp e) (/= e 0.0))
      (progn
        (setq saved (cons (cons obj e) saved))
        (vl-catch-all-apply 'vla-put-Elevation (list obj 0.0)))))
  saved)

(defun opor-tiles-restore-elevation (saved)
  (foreach pair saved
    (vl-catch-all-apply 'vla-put-Elevation (list (car pair) (cdr pair)))))

;; ковёр плиток по индекс-диапазонам сетки (позиции линий обеих семей);
;; лишние позиции запаса диапазона удалит классификация по контуру
(defun opor-tiles-carpet (base vec perp sx sy range-i range-j / i j origin tiles)
  (setq tiles '())
  (setq i (car range-i))
  (while (<= i (cadr range-i))
    (setq j (car range-j))
    (while (<= j (cadr range-j))
      (setq origin (opor-v+ base (opor-v+ (opor-v* vec (* i sx)) (opor-v* perp (* j sy)))))
      (setq tiles (cons (opor-tile-polyline origin vec perp sx sy) tiles))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (reverse tiles))

;; Раскладка доски для ведомости. Доски считаются той же проверенной Boolean-
;; геометрией, что плитка, но после подсчёта временные объекты удаляются.
;; При раскладке 1/2 соседние ряды сдвинуты на половину длины доски.
(defun opor-boards-carpet
  (base axis width-axis board-length board-width range-i range-j stagger-p
   / i j shift origin boards)
  (setq boards '())
  (setq i (car range-i))
  (while (<= i (cadr range-i))
    (setq j (car range-j))
    (while (<= j (cadr range-j))
      (setq shift
        (if (and stagger-p (/= (rem (abs j) 2) 0))
          (/ board-length 2.0)
          0.0))
      (setq origin
        (opor-v+ base
          (opor-v+
            (opor-v* axis (+ (* i board-length) shift))
            (opor-v* width-axis (* j board-width)))))
      (setq boards
        (cons
          (opor-tile-polyline
            origin axis width-axis board-length board-width)
          boards))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (reverse boards))

(defun opor-coverage-delete-results (split / obj)
  (foreach obj (append (car split) (cdr split))
    (opor-delete-object obj)
    (opor-unregister-created obj))
  T)

;; del_plitk + trimplitk: классификация по контуру и резка пересекающих.
;; Возвращает (список полилиний . список регионов)
(defun opor-tiles-clip-contour (tiles boundary / polys regions center plarea creg oreg
                                cregarea lst raw)
  (setq polys '())
  (setq regions '())
  (foreach pline tiles
    (if (opor-obj-intersections pline boundary)
      ;; intersc: регион контура заново на каждую плитку (Boolean его мутирует)
      (progn
        (setq plarea (opor-tile-area pline))
        (setq creg (opor-tile-region boundary))
        (setq oreg (if creg (opor-tile-region pline) nil))
        (cond
          ((not oreg)
            ;; регион не построился - плитку оставляем целой (guard)
            (if creg (opor-tile-delete-quiet creg))
            (setq polys (cons pline polys)))
          ((not (opor-tile-boolean creg 1 oreg))
            (opor-tile-delete-quiet oreg)
            (opor-tile-delete-quiet creg)
            (setq polys (cons pline polys)))
          ((< (setq cregarea (opor-tile-area creg)) *opor-vba-tile-outside-area-tolerance*)
            ;; плитка снаружи (касание извне)
            (opor-tile-delete-quiet creg)
            (opor-delete-object pline)
            (opor-unregister-created pline))
          ((<= (- plarea cregarea) *opor-vba-tile-trim-area-tolerance*)
            ;; касание изнутри: полилиния остаётся целой
            (opor-tile-delete-quiet creg)
            (setq polys (cons pline polys)))
          (t
            ;; обрезалась: полилиния заменяется регионом (или несколькими при распаде)
            (setq raw (vl-catch-all-apply 'vlax-invoke (list creg 'Explode)))
            (setq lst (if (vl-catch-all-error-p raw) nil raw))
            (if (and lst (= (vla-get-ObjectName (car lst)) "AcDbRegion"))
              (progn
                ;; распад: куски explode = регионы-плитки, исходный регион удаляется
                (foreach r lst
                  (vla-put-Layer r *opor-layer-tiles*)
                  (opor-register-created r "tile-region")
                  (setq regions (cons r regions)))
                (opor-tile-delete-quiet creg))
              (progn
                ;; один кусок: creg сам становится плиткой, explode-кривые удаляются
                (foreach e lst (opor-tile-delete-quiet e))
                (vla-put-Layer creg *opor-layer-tiles*)
                (opor-register-created creg "tile-region")
                (setq regions (cons creg regions))))
            (opor-delete-object pline)
            (opor-unregister-created pline))))
      ;; нет пересечения с контуром: по центру - внутри или вне
      (progn
        (setq center (opor-tile-center pline))
        (if (or (opor-point-inside-boundary-p center boundary)
                (opor-point-on-curve-p center boundary *opor-point-tolerance*))
          (setq polys (cons pline polys))
          (progn
            (opor-delete-object pline)
            (opor-unregister-created pline))))))
  (cons (reverse polys) (reverse regions)))

;; trimPlitkOpp с Б-фиксом: режем только плитки, реально задетые проёмом
(defun opor-tiles-cut-holes (polys regions holes / hole-bb kept-p kept-r preg hreg
                             area-old area-new)
  (foreach hole holes
    (setq hole-bb (opor-bbox hole))
    ;; целые плитки-полилинии
    (setq kept-p '())
    (foreach pline polys
      (cond
        ((opor-obj-intersections pline hole)
          ;; задета проёмом: регион плитки минус регион проёма
          (setq area-old (opor-tile-area pline))
          (setq preg (opor-tile-region pline))
          (setq hreg (if preg (opor-tile-region hole) nil))
          (cond
            ((not hreg)
              (if preg (opor-tile-delete-quiet preg))
              (setq kept-p (cons pline kept-p)))
            ((not (opor-tile-boolean preg 2 hreg))
              (opor-tile-delete-quiet hreg)
              (opor-tile-delete-quiet preg)
              (setq kept-p (cons pline kept-p)))
            ((< (setq area-new (opor-tile-area preg)) *opor-vba-tile-hole-zero-area-tolerance*)
              ;; плитка съедена проёмом целиком
              (opor-tile-delete-quiet preg)
              (opor-delete-object pline)
              (opor-unregister-created pline))
            ((> (- area-old area-new) *opor-vba-tile-trim-area-tolerance*)
              ;; обрезалась: полилиния -> регион
              (vla-put-Layer preg *opor-layer-tiles*)
              (opor-register-created preg "tile-region")
              (setq regions (cons preg regions))
              (opor-delete-object pline)
              (opor-unregister-created pline))
            (t
              ;; краевое касание: без изменений; временный регион удаляем
              ;; (VBA здесь оставлял регион-двойник - баг, НЕ воспроизводим)
              (opor-tile-delete-quiet preg)
              (setq kept-p (cons pline kept-p)))))
        ((opor-point-inside-boundary-p (opor-tile-center pline) hole)
          ;; целиком внутри проёма
          (opor-delete-object pline)
          (opor-unregister-created pline))
        (t (setq kept-p (cons pline kept-p)))))
    (setq polys (reverse kept-p))
    ;; регионы-плитки (уже обрезанные контуром)
    (setq kept-r '())
    (foreach reg regions
      (if (opor-bbox-overlap-p (opor-bbox reg) hole-bb)
        (progn
          (setq hreg (opor-tile-region hole))
          (cond
            ((not hreg) (setq kept-r (cons reg kept-r)))
            ((not (opor-tile-boolean reg 2 hreg))
              (opor-tile-delete-quiet hreg)
              (setq kept-r (cons reg kept-r)))
            ((< (opor-tile-area reg) *opor-vba-tile-hole-zero-area-tolerance*)
              (opor-delete-object reg)
              (opor-unregister-created reg))
            (t (setq kept-r (cons reg kept-r)))))
        (setq kept-r (cons reg kept-r))))
    (setq regions (reverse kept-r)))
  (cons polys regions))

;; главный вход П8: опорная сетка уже рассчитана по step-x/step-y, а ковёр
;; плитки получает собственные размеры и собственные диапазоны от bbox.
(defun opor-tiles-run (session / boundary holes base dir vec perp sx sy bbox range-i range-j
                       saved-elev tiles split)
  (setq boundary (opor-session-get 'outer-boundary))
  (setq holes (opor-session-get 'holes))
  (setq base (opor-session-get 'base-point))
  (setq dir (opor-session-get 'direction-point))
  (setq sx (opor-session-get 'tile-size-x))
  (setq sy (opor-session-get 'tile-size-y))
  (setq vec (opor-unit (opor-v- dir base)))
  (if (and boundary base vec (numberp sx) (> sx 0.0) (numberp sy) (> sy 0.0))
    (progn
      (setq perp (opor-perp2d vec))
      (setq bbox (opor-bbox boundary))
      (setq range-i (opor-index-range-for-bbox bbox base vec sx))
      (setq range-j (opor-index-range-for-bbox bbox base perp sy))
      (opor-session-set 'tile-x-index-range range-i)
      (opor-session-set 'tile-y-index-range range-j)
      (opor-ensure-layer *opor-layer-tiles* 9 "Continuous")
      (setq saved-elev (opor-tiles-zero-elevation (cons boundary holes)))
      (setq tiles (opor-tiles-carpet base vec perp sx sy range-i range-j))
      (setq split (opor-tiles-clip-contour tiles boundary))
      (setq split (opor-tiles-cut-holes (car split) (cdr split) holes))
      (opor-tiles-restore-elevation saved-elev)
      (opor-session-set 'tile-whole-count (length (car split)))
      (opor-session-set 'tile-trimmed-count (length (cdr split)))
      T)
    (progn
      (opor-log "Плитка: не заданы положительные размеры плитки.")
      nil)))

;; В текущем чертеже доски не создаются как постоянные объекты, поэтому для
;; четырёх новых строк таблицы строим временный ковёр: длина идёт поперёк лаг,
;; ширина — вдоль направления лаг. Контур и проёмы режут доски так же, как
;; плитку; после классификации вся временная геометрия удаляется.
(defun opor-boards-count-run
  (session / boundary holes base dir vec perp lag-axis axis width-axis
           board-length board-width layout bbox range-i range-j saved-elev
           boards split)
  (setq boundary (opor-session-get 'outer-boundary))
  (setq holes (opor-session-get 'holes))
  (setq base (opor-session-get 'base-point))
  (setq dir (opor-session-get 'direction-point))
  (setq board-length (opor-session-get 'board-length))
  (setq board-width (opor-session-get 'board-width))
  (setq layout (opor-session-get 'double-lag-layout))
  (setq lag-axis (opor-session-get 'lag-axis))
  (setq vec (opor-unit (opor-v- dir base)))
  (if (and boundary base vec
           (numberp board-length) (> board-length 0.0)
           (numberp board-width) (> board-width 0.0))
    (progn
      (setq perp (opor-perp2d vec))
      (if (= lag-axis "perp")
        (setq axis vec width-axis perp)
        (setq axis perp width-axis vec))
      (setq bbox (opor-bbox boundary))
      (setq range-i
        (opor-index-range-for-bbox bbox base axis board-length))
      (setq range-j
        (opor-index-range-for-bbox bbox base width-axis board-width))
      (opor-session-set 'board-length-index-range range-i)
      (opor-session-set 'board-width-index-range range-j)
      (opor-ensure-layer *opor-layer-tiles* 9 "Continuous")
      (setq saved-elev (opor-tiles-zero-elevation (cons boundary holes)))
      (setq boards
        (opor-boards-carpet
          base axis width-axis board-length board-width range-i range-j
          (= layout "half")))
      (setq split (opor-tiles-clip-contour boards boundary))
      (setq split (opor-tiles-cut-holes (car split) (cdr split) holes))
      (opor-tiles-restore-elevation saved-elev)
      (opor-session-set 'board-whole-count (length (car split)))
      (opor-session-set 'board-trimmed-count (length (cdr split)))
      (opor-coverage-delete-results split)
      T)
    (progn
      (opor-session-set 'board-whole-count 0)
      (opor-session-set 'board-trimmed-count 0)
      (opor-log "Доска: не заданы положительные длина и ширина.")
      nil)))

(defun opor-tiles-qc (/ n)
  (setq n (opor-session-get 'tile-whole-count))
  (if (numberp n) n 0))

(defun opor-tiles-qr (/ n)
  (setq n (opor-session-get 'tile-trimmed-count))
  (if (numberp n) n 0))

(defun opor-boards-qc (/ n)
  (setq n (opor-session-get 'board-whole-count))
  (if (numberp n) n 0))

(defun opor-boards-qr (/ n)
  (setq n (opor-session-get 'board-trimmed-count))
  (if (numberp n) n 0))

(defun opor-tiles-log-text ()
  (if (= (opor-session-get 'tile-mode) "p")
    (strcat
      ", плитка="
      (rtos (opor-session-get 'tile-size-x) 2 0) "×"
      (rtos (opor-session-get 'tile-size-y) 2 0)
      ", плитки: целых=" (itoa (opor-tiles-qc))
      ", обрезанных=" (itoa (opor-tiles-qr)))
    ""))

(princ)
