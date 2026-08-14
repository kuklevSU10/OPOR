;;; OPOR geometry and AutoCAD object helpers

(defun opor-to-vla (value)
  (cond
    ((= (type value) 'VLA-OBJECT) value)
    ((= (type value) 'ENAME) (vlax-ename->vla-object value))
    (t nil)))

(defun opor-obj-name (obj)
  (if obj (vla-get-ObjectName obj) ""))

(defun opor-polyline-object-p (obj / name)
  (setq name (opor-obj-name obj))
  (or (= name "AcDbPolyline")
      (= name "AcDb2dPolyline")
      (= name "AcDb3dPolyline")))

(defun opor-line-object-p (obj)
  (= (opor-obj-name obj) "AcDbLine"))

(defun opor-block-exists-p (name)
  (if (tblsearch "BLOCK" name) T nil))

(defun opor-layer-exists-p (name)
  (if (tblsearch "LAYER" name) T nil))

;; существующие слои НЕ перекрашиваем — цвет/тип только новым
(defun opor-ensure-layer (name color linetype / layers layer existed)
  (setq layers (vla-get-Layers (opor-doc)))
  (setq existed (opor-layer-exists-p name))
  (if existed
    (setq layer (vla-Item layers name))
    (progn
      (setq layer (vla-Add layers name))
      (if color (vl-catch-all-apply 'vla-put-Color (list layer color)))
      (if linetype (vl-catch-all-apply 'vla-put-Linetype (list layer linetype)))))
  (vl-catch-all-apply 'vla-put-Freeze (list layer :vlax-false))
  (vl-catch-all-apply 'vla-put-LayerOn (list layer :vlax-true))
  layer)

(defun opor-ensure-layers ()
  (foreach def *opor-layer-defs*
    (opor-ensure-layer (car def) (cadr def) (caddr def)))
  T)

(defun opor-v+ (a b)
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)) (+ (caddr a) (caddr b))))

(defun opor-v- (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr a) (caddr b))))

(defun opor-v* (a s)
  (list (* (car a) s) (* (cadr a) s) (* (caddr a) s)))

(defun opor-dot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)) (* (caddr a) (caddr b))))

(defun opor-dist (a b)
  (distance a b))

(defun opor-unit (a / d)
  (setq d (distance '(0.0 0.0 0.0) a))
  (if (equal d 0.0 1e-12)
    nil
    (opor-v* a (/ 1.0 d))))

(defun opor-perp2d (v)
  (list (- (cadr v)) (car v) 0.0))

(defun opor-2d (pt)
  (list (car pt) (cadr pt) 0.0))

(defun opor-safearray->points (arr / lst res)
  (setq res '())
  (if (and (= (type arr) 'safearray)
           (>= (vlax-safearray-get-u-bound arr 1)
               (vlax-safearray-get-l-bound arr 1)))
    (progn
      (setq lst (vlax-safearray->list arr))
      (while (and lst (cddr lst))
        (setq res (cons (list (car lst) (cadr lst) (caddr lst)) res))
        (setq lst (cdddr lst)))))
  (reverse res))

(defun opor-obj-intersections (o1 o2 / result)
  (if (not (boundp 'acExtendNone)) (setq acExtendNone 0))
  (setq result
    (vl-catch-all-apply
      '(lambda ()
         (opor-safearray->points
           (vlax-variant-value (vla-IntersectWith o1 o2 acExtendNone))))))
  (if (vl-catch-all-error-p result) '() result))

(defun opor-bbox (obj / ll ur r)
  (setq r
    (vl-catch-all-apply
      '(lambda ()
         (vla-GetBoundingBox obj 'll 'ur)
         (list (vlax-safearray->list ll) (vlax-safearray->list ur)))))
  (if (vl-catch-all-error-p r) nil r))

(defun opor-bbox-diagonal (bbox)
  (if bbox (distance (car bbox) (cadr bbox)) 0.0))

(defun opor-curve-start (obj)
  (vlax-curve-getStartPoint obj))

(defun opor-curve-end (obj)
  (vlax-curve-getEndPoint obj))

;; Длина любой поддерживаемой AutoCAD-кривой. Для замкнутого внешнего контура
;; это периметр объекта; COM-ошибка не должна останавливать основной расчёт.
(defun opor-curve-length (obj / value)
  (setq value
    (vl-catch-all-apply
      '(lambda ()
         (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))))
  (if (and (not (vl-catch-all-error-p value)) (numberp value))
    value
    0.0))

(defun opor-polyline-closed-p (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-Closed (list obj)))
  (if (vl-catch-all-error-p value) nil (equal value :vlax-true)))

(defun opor-polyline-points (obj / start end idx pts)
  (setq pts '())
  (setq start (fix (vlax-curve-getStartParam obj)))
  (setq end (fix (vlax-curve-getEndParam obj)))
  (setq idx start)
  (while (<= idx end)
    (setq pts (cons (vlax-curve-getPointAtParam obj idx) pts))
    (setq idx (1+ idx)))
  (reverse pts))

;; Вершины как в VBA pl.Coordinates: у замкнутой полилинии
;; без повтора стартовой точки в конце.
(defun opor-polyline-vertices (obj / pts last first)
  (setq pts (opor-polyline-points obj))
  (if (and (cdr pts) (opor-polyline-closed-p obj))
    (progn
      (setq first (car pts))
      (setq last (opor-list-tail pts))
      (if (< (distance (opor-2d first) (opor-2d last)) 1e-8)
        (setq pts (reverse (cdr (reverse pts)))))))
  pts)

;; Алгоритмы IntersectWith/Region работают с дугами полилинии напрямую, но TIN
;; и h_triang требуют прямолинейное кольцо. Разбиваем только реальные bulge-
;; сегменты; прямые контуры возвращаются с прежними координатами один в один.
(defun opor-ceiling-positive (value / whole)
  (setq whole (fix value))
  (if (> value (float whole)) (1+ whole) whole))

(defun opor-polyline-segment-bulge (obj index / value)
  (setq value (vl-catch-all-apply 'vla-GetBulge (list obj index)))
  (if (and (not (vl-catch-all-error-p value)) (numberp value)) value 0.0))

(defun opor-polyline-has-arc-segments-p (obj / start end index found)
  (setq start (fix (vlax-curve-getStartParam obj)))
  (setq end (fix (vlax-curve-getEndParam obj)))
  (setq index start found nil)
  (while (and (< index end) (not found))
    (if (not (equal (opor-polyline-segment-bulge obj index) 0.0 1e-12))
      (setq found T))
    (setq index (1+ index)))
  found)

(defun opor-polyline-arc-piece-count
  (obj index tolerance / bulge theta p1 p2 chord radius ratio max-angle pieces limit)
  (setq bulge (abs (opor-polyline-segment-bulge obj index)))
  (if (or (equal bulge 0.0 1e-12)
          (not (numberp tolerance))
          (<= tolerance 0.0))
    1
    (progn
      (setq theta (* 4.0 (atan bulge)))
      (setq p1 (vlax-curve-getPointAtParam obj index))
      (setq p2 (vlax-curve-getPointAtParam obj (1+ index)))
      (setq chord (distance (opor-2d p1) (opor-2d p2)))
      (if (or (equal chord 0.0 1e-12)
              (equal (sin (/ theta 2.0)) 0.0 1e-12))
        1
        (progn
          (setq radius (/ chord (* 2.0 (sin (/ theta 2.0)))))
          ;; cos(max-angle/2)=1-tolerance/radius. atan(y,x) заменяет acos,
          ;; которого нет в части поддерживаемых версий AutoLISP.
          (setq ratio (- 1.0 (/ tolerance radius)))
          (cond
            ((<= ratio -1.0) (setq max-angle (* 2.0 pi)))
            ((>= ratio 1.0) (setq max-angle 1e-6))
            (t
              (setq max-angle
                (* 2.0
                  (atan
                    (sqrt (max 0.0 (- 1.0 (* ratio ratio))))
                    ratio)))))
          (setq pieces
            (max 1 (opor-ceiling-positive (/ theta max-angle))))
          (setq limit
            (if (and (boundp '*opor-curve-max-pieces-per-segment*)
                     (numberp *opor-curve-max-pieces-per-segment*)
                     (> *opor-curve-max-pieces-per-segment* 0))
              (fix *opor-curve-max-pieces-per-segment*)
              720))
          (min pieces limit))))))

(defun opor-polyline-linearized-vertices (obj tolerance / start end index pieces part point result)
  (if (not (opor-polyline-has-arc-segments-p obj))
    (opor-polyline-vertices obj)
    (progn
      (setq start (fix (vlax-curve-getStartParam obj)))
      (setq end (fix (vlax-curve-getEndParam obj)))
      (setq index start result '())
      (while (< index end)
        (setq pieces (opor-polyline-arc-piece-count obj index tolerance))
        (setq part 0)
        (while (< part pieces)
          (setq point
            (vlax-curve-getPointAtParam
              obj (+ index (/ (float part) pieces))))
          (setq result (cons (opor-2d point) result))
          (setq part (1+ part)))
        (setq index (1+ index)))
      (if (not (opor-polyline-closed-p obj))
        (setq result
          (cons
            (opor-2d (vlax-curve-getPointAtParam obj end))
            result)))
      (reverse result))))

(defun opor-make-temp-ray (pt / far ray)
  (setq far (list (+ (car pt) 1.0e7) (+ (cadr pt) 3.71e6) 0.0))
  (setq ray (vla-AddLine (opor-ms) (vlax-3d-point pt) (vlax-3d-point far)))
  ray)

(defun opor-point-inside-boundary-p (pt boundary / ray count)
  (setq ray (opor-make-temp-ray (opor-2d pt)))
  (setq count (length (opor-obj-intersections ray boundary)))
  (vl-catch-all-apply 'vla-Delete (list ray))
  (= 1 (rem count 2)))

(defun opor-point-in-holes-p (pt holes)
  (vl-some '(lambda (hole) (opor-point-inside-boundary-p pt hole)) holes))

(defun opor-point-on-curve-p (pt curve tol / closest)
  (setq closest
    (vl-catch-all-apply
      '(lambda () (vlax-curve-getClosestPointTo curve (opor-2d pt)))))
  (if (vl-catch-all-error-p closest)
    nil
    (<= (distance (opor-2d pt) (opor-2d closest)) tol)))

(defun opor-point-in-working-area-p (pt boundary holes)
  (and
    (or
      (opor-point-inside-boundary-p pt boundary)
      (opor-point-on-curve-p pt boundary *opor-point-tolerance*))
    (not
      (vl-some
        '(lambda (hole)
           (and
             (opor-point-inside-boundary-p pt hole)
             (not (opor-point-on-curve-p pt hole *opor-point-tolerance*))))
        holes))))

(defun opor-region-probe (obj / raw regions region message)
  (setq raw
    (vl-catch-all-apply
      'vlax-invoke
      (list (opor-ms) 'AddRegion (list obj))))
  (if (vl-catch-all-error-p raw)
    (list nil (vl-catch-all-error-message raw))
    (progn
      (setq regions
        (cond
          ((= (type raw) 'LIST) raw)
          ((= (type raw) 'VLA-OBJECT) (list raw))
          (t (opor-variant-list raw))))
      ;; chk_reg удаляет временный регион; порт чистит все элементы массива,
      ;; чтобы при неожиданном множественном результате ничего не оставить.
      (foreach region regions
        (vl-catch-all-apply 'vla-Delete (list region)))
      (if regions
        (list T nil)
        (list nil "AddRegion не вернул регион.")))))

(defun opor-region-invalid-input-p (message / upper)
  (setq upper (strcase (opor-string message)))
  (or
    (wcmatch upper "*INVALID INPUT*")
    (wcmatch upper "*НЕВЕРН*ВВОД*")
    (wcmatch upper "*НЕДОПУСТИМ*")))

(defun opor-polyline-region-valid-p (obj object-label / result message)
  (setq result (opor-region-probe obj))
  (if (car result)
    T
    (progn
      (setq message (cadr result))
      (if (opor-region-invalid-input-p message)
        (progn
          (opor-log (strcat "chk_reg: отклонён " object-label " — самопересечение."))
          (opor-alert
            (strcat
              object-label " выполнен из полилинии с самопересечением."
              "\n\nНеобходимо перерисовать " object-label ".")))
        (opor-alert
          (strcat
            "Ошибка проверки " object-label ".\n"
            (opor-string message))))
      nil)))

(defun opor-select-outer-boundary (/ picked obj answer candidate)
  (setq picked
    (entsel
      (if (> (if (opor-session-get 'contour-count) (opor-session-get 'contour-count) 0) 0)
        "\nВыберите следующий внешний контур OPOR или Enter для завершения: "
        "\nВыберите внешний контур OPOR: ")))
  (if picked
    (progn
      (setq obj (vlax-ename->vla-object (car picked)))
      (setq candidate
        (cond
          ((not (opor-polyline-object-p obj))
            (opor-alert "Выбранный объект не является полилинией.")
            nil)
          ((not (opor-polyline-closed-p obj))
            ;; В VBA chk_reg стоит до этого вопроса. Порт сначала штатно
            ;; замыкает контур, затем проверяет уже пригодную для Region кривую.
            (initget "Да Нет")
            (setq answer (getkword "\nПолилиния не замкнута. Замкнуть? [Да/Нет] <Да>: "))
            (if (or (not answer) (= answer "Да"))
              (progn
                (vl-catch-all-apply 'vla-put-Closed (list obj :vlax-true))
                (if (opor-polyline-closed-p obj)
                  obj
                  (progn
                    (opor-alert "Не удалось замкнуть полилинию.")
                    nil)))
              nil))
          (t obj)))
      (if (and candidate (opor-polyline-region-valid-p candidate "Контур"))
        candidate
        nil))
    nil))

;; ТЗ П9: площадь спецификации = контур минус проёмы (областиvb)
(defun opor-net-area (boundary holes / area value)
  (setq area (vla-get-Area boundary))
  (foreach hole holes
    (setq value (vl-catch-all-apply 'vla-get-Area (list hole)))
    (if (not (vl-catch-all-error-p value))
      (setq area (- area value))))
  area)

(defun opor-select-holes (/ ss idx holes obj)
  (setq holes '())
  (princ "\nВыберите проёмы на слое областиvb или нажмите Enter, если проёмов нет: ")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 8 *opor-layer-holes*))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss idx)))
        (if (and (opor-polyline-object-p obj) (opor-polyline-closed-p obj))
          (setq holes (cons obj holes)))
        (setq idx (1+ idx)))))
  (reverse holes))

;; UX v3.7: проёмы не выбираются руками. Одних вершин внутри
;; недостаточно для вогнутого внешнего контура: ребро проёма может
;; выйти наружу между двумя вершинами. Такой проём не вычитаем из площади.
(defun opor-safe-object-handle (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-Handle (list obj)))
  (if (vl-catch-all-error-p value) "?" value))

(defun opor-sort-unique-distances (values / sorted result value)
  (setq sorted (vl-sort values '<))
  (setq result '())
  (foreach value sorted
    (if (or (not result)
            (> (abs (- value (car result))) *opor-point-tolerance*))
      (setq result (cons value result))))
  (reverse result))

(defun opor-curve-distance-at-point-safe (curve pt / value closest)
  (setq value
    (vl-catch-all-apply
      '(lambda () (vlax-curve-getDistAtPoint curve (opor-2d pt)))))
  (if (and (not (vl-catch-all-error-p value)) (numberp value))
    value
    (progn
      (setq closest
        (vl-catch-all-apply
          '(lambda () (vlax-curve-getClosestPointTo curve (opor-2d pt)))))
      (if (vl-catch-all-error-p closest)
        nil
        (progn
          (setq value
            (vl-catch-all-apply
              '(lambda () (vlax-curve-getDistAtPoint curve closest))))
          (if (and (not (vl-catch-all-error-p value)) (numberp value))
            value
            nil))))))

;; IntersectWith возвращает точки и для допустимого касания. Чтобы отличить
;; его от реального выхода проёма наружу, делим замкнутую кривую всеми точками
;; контакта и проверяем середину каждого получившегося интервала. Между двумя
;; соседними пересечениями принадлежность области не меняется; общая часть
;; границы считается допустимой, поскольку точка лежит непосредственно на ней.
(defun opor-curve-contained-in-boundary-p
  (curve boundary intersections / total distances failed dist current next mid pt contained)
  (setq total (opor-curve-length curve))
  (setq distances (list 0.0 total) failed nil)
  (foreach pt intersections
    (setq dist (opor-curve-distance-at-point-safe curve pt))
    (if (numberp dist)
      (setq distances (cons dist distances))
      (setq failed T)))
  (if (or failed (<= total *opor-point-tolerance*))
    nil
    (progn
      (setq distances (opor-sort-unique-distances distances))
      (setq contained T)
      (while (and contained (cdr distances))
        (setq current (car distances) next (cadr distances))
        (if (> (- next current) *opor-point-tolerance*)
          (progn
            (setq mid (/ (+ current next) 2.0))
            (setq pt
              (vl-catch-all-apply
                '(lambda () (vlax-curve-getPointAtDist curve mid))))
            (if (or (vl-catch-all-error-p pt)
                    (not
                      (or
                        (opor-point-inside-boundary-p (opor-2d pt) boundary)
                        (opor-point-on-curve-p
                          (opor-2d pt) boundary *opor-point-tolerance*))))
              (setq contained nil))))
        (setq distances (cdr distances)))
      contained)))

(defun opor-detect-holes
  (boundary / holes obj inside contained boundary-errors boundary-conflicts pt crossings boundary-point)
  (setq holes '() boundary-errors 0 boundary-conflicts '())
  (setq boundary-point (car (opor-polyline-vertices boundary)))
  (vlax-for obj (opor-ms)
    (if (and
          (= (vla-get-ObjectName obj) "AcDbPolyline")
          (= (strcase (vla-get-Layer obj)) (strcase *opor-layer-holes*))
          (opor-polyline-closed-p obj)
          (not (equal obj boundary)))
      (progn
        (setq inside T)
        (foreach pt (opor-polyline-vertices obj)
          (if (and inside
                   (not (opor-point-inside-boundary-p (opor-2d pt) boundary))
                   (not (opor-point-on-curve-p (opor-2d pt) boundary *opor-point-tolerance*)))
            (setq inside nil)))
        (setq crossings (opor-obj-intersections obj boundary))
        (setq contained
          (and inside
               (or (not crossings)
                   (opor-curve-contained-in-boundary-p obj boundary crossings))))
        (cond
          (contained
            (setq holes (cons obj holes)))
          ((or crossings
                (and boundary-point
                     (opor-point-inside-boundary-p boundary-point obj)))
            (setq boundary-errors (1+ boundary-errors))
            (setq boundary-conflicts (cons obj boundary-conflicts)))))))
  (setq holes (reverse holes))
  (setq boundary-conflicts (reverse boundary-conflicts))
  (if (and (boundp '*opor-session*) *opor-session*)
    (progn
      (opor-session-set 'hole-boundary-errors boundary-errors)
      (opor-session-set 'hole-boundary-conflicts boundary-conflicts)))
  (opor-log (strcat "Проёмов найдено: " (itoa (length holes))))
  (if (> boundary-errors 0)
    (progn
      (opor-log
        (strcat "Проёмов, пересекающих внешний контур: "
          (itoa boundary-errors) "."))
      (foreach obj boundary-conflicts
        (setq crossings (opor-obj-intersections obj boundary))
        (opor-log
          (strcat "Конфликтный проём: handle="
            (opor-safe-object-handle obj)
            ", точек пересечения="
            (itoa (length crossings)) "."))
        (foreach pt crossings
          (opor-log
            (strcat "Точка конфликта: X=" (rtos (car pt) 2 3)
              ", Y=" (rtos (cadr pt) 2 3) "."))))))
  holes)

;; Валидация должна не только сообщить число конфликтов, но и показать их.
;; Для реального пересечения отмечаем точные точки IntersectWith. Если проём
;; целиком охватывает внешний контур, пересечений нет — ставим маркер в первой
;; вершине конфликтного проёма, чтобы пользователь всё равно увидел объект.
(defun opor-hole-boundary-conflict-points (boundary conflicts / points hole crossings vertices)
  (setq points '())
  (foreach hole conflicts
    (setq crossings (opor-obj-intersections hole boundary))
    (if crossings
      (setq points (append crossings points))
      (progn
        (setq vertices (opor-polyline-vertices hole))
        (if vertices (setq points (cons (car vertices) points))))))
  (opor-unique-points points *opor-point-tolerance*))

(defun opor-show-hole-boundary-conflicts (/ boundary conflicts points)
  (setq boundary
    (if (and (boundp '*opor-session*) *opor-session*)
      (opor-session-get 'outer-boundary)
      nil))
  (setq conflicts
    (if (and (boundp '*opor-session*) *opor-session*)
      (opor-session-get 'hole-boundary-conflicts)
      nil))
  (setq points
    (if (and boundary conflicts)
      (opor-hole-boundary-conflict-points boundary conflicts)
      '()))
  (if points
    (progn
      (opor-error-circles points)
      (opor-zoom-to-points points)))
  points)

(defun opor-holes-overlap-p (a b / apt bpt)
  (setq apt (car (opor-polyline-vertices a)))
  (setq bpt (car (opor-polyline-vertices b)))
  (or
    (opor-obj-intersections a b)
    (and apt
         (or (opor-point-inside-boundary-p apt b)
             (opor-point-on-curve-p apt b *opor-point-tolerance*)))
    (and bpt
         (or (opor-point-inside-boundary-p bpt a)
             (opor-point-on-curve-p bpt a *opor-point-tolerance*)))))

(defun opor-hole-overlap-count (holes / count remaining hole other)
  (setq count 0 remaining holes)
  (while remaining
    (setq hole (car remaining))
    (foreach other (cdr remaining)
      (if (opor-holes-overlap-p hole other)
        (setq count (1+ count))))
    (setq remaining (cdr remaining)))
  count)

(defun opor-hole-overlap-pairs (holes / pairs remaining hole other)
  (setq pairs '() remaining holes)
  (while remaining
    (setq hole (car remaining))
    (foreach other (cdr remaining)
      (if (opor-holes-overlap-p hole other)
        (setq pairs (cons (list hole other) pairs))))
    (setq remaining (cdr remaining)))
  (reverse pairs))

;; У пересекающейся пары показываем точные пересечения. Для вложенной пары
;; берём вершину внутреннего проёма — это также однозначная проблемная точка.
(defun opor-hole-overlap-points (pairs / points pair a b crossings apt bpt)
  (setq points '())
  (foreach pair pairs
    (setq a (car pair) b (cadr pair))
    (setq crossings (opor-obj-intersections a b))
    (if crossings
      (setq points (append crossings points))
      (progn
        (setq apt (car (opor-polyline-vertices a)))
        (setq bpt (car (opor-polyline-vertices b)))
        (cond
          ((and apt
                (or (opor-point-inside-boundary-p apt b)
                    (opor-point-on-curve-p apt b *opor-point-tolerance*)))
            (setq points (cons apt points)))
          (bpt (setq points (cons bpt points)))))))
  (opor-unique-points points *opor-point-tolerance*))

(defun opor-hole-regions-valid-p
  (holes / valid index hole boundary-errors overlaps conflict-points overlap-pairs overlap-points)
  (setq valid T index 1)
  (setq boundary-errors
    (if (and (boundp '*opor-session*) *opor-session*)
      (opor-session-get 'hole-boundary-errors)
      0))
  (if (not (numberp boundary-errors)) (setq boundary-errors 0))
  (if (> boundary-errors 0)
    (progn
      (setq conflict-points (opor-show-hole-boundary-conflicts))
      (opor-alert
        (strcat
          "Найдены проёмы, пересекающие внешний контур.\n"
          "Исправьте геометрию проёмов: " (itoa boundary-errors) ".\n"
          "Проблемные места отмечены оранжевыми кругами: "
          (itoa (length conflict-points)) "."))
      (setq valid nil)))
  (foreach hole holes
    (if valid
      (if (not
            (opor-polyline-region-valid-p
              hole (strcat "Проём " (itoa index))))
        (setq valid nil)))
    (setq index (1+ index)))
  (if valid
    (progn
      (setq overlap-pairs (opor-hole-overlap-pairs holes))
      (setq overlaps (length overlap-pairs))
      (if (> overlaps 0)
        (progn
          (setq overlap-points (opor-hole-overlap-points overlap-pairs))
          (if overlap-points
            (progn
              (opor-error-circles overlap-points)
              (opor-zoom-to-points overlap-points)))
          (opor-alert
            (strcat
              "Проёмы пересекаются или вложены друг в друга.\n"
              "Проблемных пар: " (itoa overlaps) ".\n"
              "Проблемные места отмечены оранжевыми кругами: "
              (itoa (length overlap-points)) "."))
          (setq valid nil)))))
  valid)

(defun opor-command-region-check (/ boundary)
  (setq boundary (opor-select-outer-boundary))
  (if boundary
    (opor-log "chk_reg: полилиния пригодна для создания Region."))
  (if boundary T nil))

;; UX v3.7: центрирование экрана на контуре (+10% отступ)
(defun opor-zoom-to-boundary (boundary / bbox ll ur dx dy)
  (setq bbox (opor-bbox boundary))
  (if bbox
    (progn
      (setq ll (car bbox))
      (setq ur (cadr bbox))
      (setq dx (* 0.1 (max 1.0 (- (car ur) (car ll)))))
      (setq dy (* 0.1 (max 1.0 (- (cadr ur) (cadr ll)))))
      (vl-catch-all-apply 'vla-ZoomWindow
        (list (vlax-get-acad-object)
              (vlax-3d-point (list (- (car ll) dx) (- (cadr ll) dy) 0.0))
               (vlax-3d-point (list (+ (car ur) dx) (+ (cadr ur) dy) 0.0)))))))

;; Крупный план набора диагностических точек. Минимальный отступ в два радиуса
;; маркера гарантирует, что оранжевый круг не обрежется даже для одной точки.
(defun opor-zoom-to-points (points / first pt minx miny maxx maxy span margin)
  (if points
    (progn
      (setq first (opor-2d (car points)))
      (setq minx (car first) maxx (car first))
      (setq miny (cadr first) maxy (cadr first))
      (foreach pt (cdr points)
        (setq pt (opor-2d pt))
        (setq minx (min minx (car pt)))
        (setq miny (min miny (cadr pt)))
        (setq maxx (max maxx (car pt)))
        (setq maxy (max maxy (cadr pt))))
      (setq span (max 1.0 (- maxx minx) (- maxy miny)))
      (setq margin
        (max (* 2.0 *opor-error-circle-radius*) (* 0.2 span)))
      (vl-catch-all-apply
        'vla-ZoomWindow
        (list
          (vlax-get-acad-object)
          (vlax-3d-point (list (- minx margin) (- miny margin) 0.0))
          (vlax-3d-point (list (+ maxx margin) (+ maxy margin) 0.0)))))))

(princ)
