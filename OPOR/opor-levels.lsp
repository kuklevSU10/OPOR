;;; OPOR variable-height support classification.
;;; Конвейер как в VBA: scan (get_arrblk + chk_blk_contr + chk_heightPLblk) ->
;;; params -> finalize (zfloor + триангуляция h_triang) -> классификация точек:
;;; вершины getH_vertecs, границы getH_bords, узлы getH_nods.

(defun opor-level-block-p (obj)
  (and (= (opor-obj-name obj) "AcDbBlockReference")
       (= (opor-effective-block-name obj) *opor-level-block-name*)))

(defun opor-ssget-crossing-objects (bbox filter / ss idx result)
  (setq result '())
  (setq ss (ssget "_C" (car bbox) (cadr bbox) filter))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq result (cons (vlax-ename->vla-object (ssname ss idx)) result))
        (setq idx (1+ idx)))))
  (reverse result))

(defun opor-level-point-in-boundary-p (pt boundary tol)
  (or
    (opor-point-inside-boundary-p (opor-2d pt) boundary)
    (opor-point-on-curve-p (opor-2d pt) boundary tol)))

(defun opor-level-read-marks (boundary / bbox objects obj pt text value color marks)
  (setq bbox (opor-bbox boundary))
  (setq objects (opor-ssget-crossing-objects bbox (list (cons 0 "INSERT"))))
  (setq marks '())
  (foreach obj objects
    (if (opor-level-block-p obj)
      (progn
        (setq pt (opor-2d (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj)))))
        ;; B.Box соседних террас может пересекаться с выбранной. Оставляем
        ;; только отметки внутри реального контура или не дальше штатного
        ;; допуска сопоставления от его границы.
        (if (opor-level-point-in-boundary-p
              pt boundary *opor-vba-mark-match-tolerance*)
          (progn
            (setq text (opor-first-attribute-text obj))
            (setq value (opor-parse-real text nil))
            (setq color (vla-get-Color obj))
            (if value
              (setq marks
                (cons
                  (list
                    (cons 'point pt)
                    (cons 'height value)
                    (cons 'color color)
                    ;; Объект нужен TIN, чтобы отличать рассчитанные им отметки
                    ;; проёмов от исходных пользовательских блоков.
                    (cons 'object obj))
                  marks))))))))
  (reverse marks))

;; Внутри первичного B.Box оставляем только полилинии, реально пересекающие
;; выбранный контур: вершина внутри/на границе, пересечение рёбер либо замкнутая
;; область охватывает вершину внешнего контура. Это изолирует соседние террасы.
(defun opor-level-polyline-overlaps-boundary-p
  (obj boundary / result pt intersections)
  (setq result nil)
  (foreach pt (opor-polyline-vertices obj)
    (if (and (not result)
             (opor-level-point-in-boundary-p pt boundary *opor-point-tolerance*))
      (setq result T)))
  (if (not result)
    (progn
      (setq intersections (opor-obj-intersections obj boundary))
      (if intersections (setq result T))))
  (if (and (not result) (opor-polyline-closed-p obj))
    (foreach pt (opor-polyline-vertices boundary)
      (if (and (not result)
               (opor-level-point-in-boundary-p pt obj *opor-point-tolerance*))
        (setq result T))))
  result)

;; VBA первоначально выбирает с 'линии_высот' любые полилинии в B.Box. После
;; этого добавлен геометрический фильтр для независимой обработки нескольких
;; соседних контуров; фильтр замкнутости по-прежнему не вводим.
(defun opor-level-read-polylines (boundary / bbox objects result obj)
  (setq bbox (opor-bbox boundary))
  (setq objects
    (opor-ssget-crossing-objects
      bbox
      (list
        (cons 0 "LWPOLYLINE,POLYLINE")
        (cons 8 *opor-layer-level-lines*))))
  (setq result '())
  (foreach obj objects
    (if (and
          (opor-polyline-object-p obj)
          (opor-level-polyline-overlaps-boundary-p obj boundary))
      (setq result (cons obj result))))
  (reverse result))

;; Квадратное окно VBA: Abs(dx)<tol And Abs(dy)<tol
(defun opor-level-mark-at-point (marks pt tol / found mp)
  (setq found nil)
  (foreach mark marks
    (if (not found)
      (progn
        (setq mp (cdr (assoc 'point mark)))
        (if (and (< (abs (- (car pt) (car mp))) tol)
                 (< (abs (- (cadr pt) (cadr mp))) tol))
          (setq found mark)))))
  found)

(defun opor-level-mark-height (marks pt / mark)
  (setq mark (opor-level-mark-at-point marks pt *opor-vba-mark-match-tolerance*))
  (if mark (cdr (assoc 'height mark)) nil))

(defun opor-level-unmarked-points (points marks / missing)
  (setq missing '())
  (foreach pt points
    (if (not (opor-level-mark-at-point marks pt *opor-vba-mark-match-tolerance*))
      (setq missing (cons pt missing))))
  (reverse missing))

;; Эти отметки — технические концы хорд дуги, созданные OPORTIN. Они нужны
;; Var для высот треугольников, но не являются конструктивными вершинами,
;; в которых обязательно ставить отдельную опору.
(defun opor-level-curve-sample-mark-p (mark / obj)
  (setq obj (cdr (assoc 'object mark)))
  (and obj
       (= (opor-object-xdata-type obj) "tin-interpolated-curve-mark")))

(defun opor-level-curve-sample-points (marks / result mark)
  (setq result '())
  (foreach mark marks
    (if (opor-level-curve-sample-mark-p mark)
      (setq result (cons (cdr (assoc 'point mark)) result))))
  (reverse result))

;; Оранжевый круг-маркер как в VBA (AddCircle цвет 30), с XData для OPORCLEAN
(defun opor-error-circle (pt / circle)
  (setq circle
    (vl-catch-all-apply
      '(lambda ()
         (vla-AddCircle (opor-ms) (vlax-3d-point (opor-2d pt)) *opor-error-circle-radius*))))
  (if (vl-catch-all-error-p circle)
    nil
    (progn
      (vl-catch-all-apply 'vla-put-Color (list circle *opor-error-color*))
      (opor-register-created circle "error-marker"))))

(defun opor-error-circles (points)
  (foreach pt points (opor-error-circle pt)))

;; --- Фаза 1: скан и валидация ДО параметров (как a_main до формы) ---
(defun opor-level-scan (session / boundary marks plines level-vertices contour-vertices curve-sample-points missing-contour missing-area)
  (setq boundary (opor-session-get 'outer-boundary))
  (setq marks (opor-level-read-marks boundary))
  (setq plines (opor-level-read-polylines boundary))
  (setq level-vertices '())
  (foreach pline plines
    (setq level-vertices (append level-vertices (opor-polyline-vertices pline))))
  (setq level-vertices (opor-unique-points level-vertices *opor-vba-point-dedupe-tolerance*))
  (setq contour-vertices
    (opor-unique-points
      (opor-polyline-vertices boundary)
      *opor-vba-point-dedupe-tolerance*))
  ;; chk_blk_contr: на каждой вершине контура есть блок отметки
  (setq missing-contour (opor-level-unmarked-points contour-vertices marks))
  ;; chk_heightPLblk: на каждой вершине областей высот есть блок отметки
  (setq missing-area (opor-level-unmarked-points level-vertices marks))
  (setq curve-sample-points (opor-level-curve-sample-points marks))
  (opor-session-set 'level-marks marks)
  (opor-session-set 'level-polylines plines)
  (opor-session-set 'level-vertex-points level-vertices)
  (opor-session-set 'level-curve-sample-points curve-sample-points)
  (opor-session-set 'level-mark-count (length marks))
  (opor-session-set 'level-polyline-count (length plines))
  (opor-session-set 'level-vertex-count (length level-vertices))
  (opor-session-set 'level-missing-mark-count (length missing-area))
  (opor-session-set 'level-missing-contour-count (length missing-contour))
  (cond
    ((not marks)
      (opor-alert "Не найдены блоки отметок.") ; get_arrblk
      nil)
    ((not plines)
      (opor-alert "Не найдены области высот.") ; chk_heightPLblk
      nil)
    (missing-contour
      (opor-error-circles missing-contour)
      (opor-alert
        (strcat
          "Нет равенства количества блоков отметок уровня\n"
          "в вершинах контура и количества вершин.\n\n"
          "Вершины отмечены окружностью оранжевого цвета: "
          (itoa (length missing-contour))))
      nil)
    (missing-area
      (opor-error-circles missing-area)
      (opor-alert
        (strcat
          "Не хватает блоков отметок уровня в вершинах областей высот.\n\n"
          "Вершины отмечены окружностью оранжевого цвета: "
          (itoa (length missing-area))))
      nil)
    (t T)))

(defun opor-level-heights (marks / result)
  (setq result '())
  (foreach mark marks
    (setq result (cons (cdr (assoc 'height mark)) result)))
  result)

(defun opor-level-max-mark (session / heights)
  (setq heights (opor-level-heights (opor-session-get 'level-marks)))
  (if heights (apply 'max heights) nil))

;; --- Фаза 2: после параметров (как b_main) ---
;; zfloor: отрицательные отметки поднимаются так, чтобы минимальная стала +100
(defun opor-level-apply-zfloor (session / marks heights minh zfloor lifted)
  (setq marks (opor-session-get 'level-marks))
  (setq heights (opor-level-heights marks))
  (setq minh (if heights (apply 'min heights) 0.0))
  (if (< minh 0.0)
    (progn
      (setq zfloor (+ (- minh) 100.0))
      (setq lifted '())
      (foreach mark marks
        (setq lifted
          (cons
            (subst
              (cons 'height (+ (cdr (assoc 'height mark)) zfloor))
              (assoc 'height mark)
              mark)
            lifted)))
      (opor-session-set 'level-marks (reverse lifted))
      (opor-session-set 'floor-height (+ (opor-session-get 'floor-height) zfloor))
      (opor-session-set 'zfloor zfloor))
    (opor-session-set 'zfloor 0.0))
  T)

(defun opor-list-index-of (item lst / idx found)
  (setq idx 0)
  (setq found nil)
  (while (and lst (not found))
    (if (eq item (car lst))
      (setq found idx)
      (progn
        (setq idx (1+ idx))
        (setq lst (cdr lst)))))
  found)

(defun opor-rotate-list (lst start / result idx n)
  (setq result '())
  (setq idx start)
  (setq n (length lst))
  (while (< (length result) n)
    (setq result (append result (list (nth (rem idx n) lst))))
    (setq idx (1+ idx)))
  result)

;; Воронка: красный блок (цвет 1), иначе минимальная отметка (h_triang)
(defun opor-level-drain-entry (entries / found min-entry)
  (setq found nil)
  (foreach entry entries
    (if (and (not found) (= (cdr (assoc 'color entry)) 1))
      (setq found entry)))
  (if found
    found
    (progn
      (setq min-entry (car entries))
      (foreach entry (cdr entries)
        (if (< (cdr (assoc 'height entry)) (cdr (assoc 'height min-entry)))
          (setq min-entry entry)))
      min-entry)))

(defun opor-triangle-area2d (a b c)
  (abs
    (* 0.5
      (+
        (* (car a) (cadr b))
        (* (car b) (cadr c))
        (* (car c) (cadr a))
        (- (* (car b) (cadr a)))
        (- (* (car c) (cadr b)))
        (- (* (car a) (cadr c)))))))

;; Высоты вершин треугольника целочисленные, как CLng в getH_nods
(defun opor-level-make-triangle (a b c)
  (list
    (cons 'a (cdr (assoc 'point a)))
    (cons 'az (opor-round-half-even (cdr (assoc 'height a))))
    (cons 'b (cdr (assoc 'point b)))
    (cons 'bz (opor-round-half-even (cdr (assoc 'height b))))
    (cons 'c (cdr (assoc 'point c)))
    (cons 'cz (opor-round-half-even (cdr (assoc 'height c))))))

;; Промежуточная точка дуги получает высоту линейной интерполяцией между
;; отметками концов сегмента. Параметр LWPOLYLINE внутри bulge-сегмента
;; пропорционален пройденной длине — та же семантика уже используется
;; getH_bords. Эти точки существуют только в расчёте и не создают блоки.
(defun opor-level-curve-entry-at-point
  (pline pt marks / mark param endp seg tval p1 p2 h1 h2)
  (setq mark (opor-level-mark-at-point marks pt *opor-vba-mark-match-tolerance*))
  (if mark
    (list
      (cons 'point (opor-2d pt))
      (cons 'height (cdr (assoc 'height mark)))
      (cons 'color (cdr (assoc 'color mark))))
    (progn
      (setq param
        (vl-catch-all-apply
          '(lambda () (vlax-curve-getParamAtPoint pline (opor-2d pt)))))
      (if (vl-catch-all-error-p param)
        nil
        (progn
          (setq endp (fix (vlax-curve-getEndParam pline)))
          (setq seg (fix param))
          (if (>= seg endp) (setq seg (1- endp)))
          (if (< seg 0) (setq seg 0))
          (setq tval (- param seg))
          (setq p1 (vlax-curve-getPointAtParam pline seg))
          (setq p2 (vlax-curve-getPointAtParam pline (1+ seg)))
          (setq h1 (opor-level-mark-height marks p1))
          (setq h2 (opor-level-mark-height marks p2))
          (if (and (numberp h1) (numberp h2))
            (list
              (cons 'point (opor-2d pt))
              (cons 'height (+ (* h1 (- 1.0 tval)) (* h2 tval)))
              (cons 'color 256))
            nil))))))

;; Вершины области: дедуп дистанцией < 1 (h_triang), привязка к отметке окном < 1.
;; Для прямых полилиний список точек остаётся прежним; для bulge-дуг добавляются
;; расчётные точки с погрешностью хорды не выше *opor-curve-chord-tolerance*.
(defun opor-level-vertex-entries (pline marks / pts entries entry)
  (setq pts
    (opor-unique-points
      (opor-polyline-linearized-vertices
        pline *opor-curve-chord-tolerance*)
      1.0))
  (setq entries '())
  (foreach pt pts
    (setq entry (opor-level-curve-entry-at-point pline pt marks))
    (if entry (setq entries (cons entry entries))))
  (reverse entries))

(defun opor-level-triangulate-poly (pline marks / entries drain start ordered result idx a b c)
  (setq entries (opor-level-vertex-entries pline marks))
  (setq result '())
  (if (>= (length entries) 3)
    (progn
      (setq drain (opor-level-drain-entry entries))
      (setq start (opor-list-index-of drain entries))
      (if (not start) (setq start 0))
      (setq ordered (opor-rotate-list entries start))
      (setq idx 1)
      (while (< idx (1- (length ordered)))
        (setq a (car ordered))
        (setq b (nth idx ordered))
        (setq c (nth (1+ idx) ordered))
        (if (> (opor-triangle-area2d
                 (cdr (assoc 'point a))
                 (cdr (assoc 'point b))
                 (cdr (assoc 'point c)))
               1.0)
          (setq result (cons (opor-level-make-triangle a b c) result)))
        (setq idx (1+ idx)))))
  (reverse result))

(defun opor-level-triangulate (session / marks plines triangles)
  (setq marks (opor-session-get 'level-marks))
  (setq plines (opor-session-get 'level-polylines))
  (setq triangles '())
  (foreach pline plines
    (setq triangles (append triangles (opor-level-triangulate-poly pline marks))))
  (opor-session-set 'level-triangles triangles)
  (opor-session-set 'level-triangle-count (length triangles))
  triangles)

;; «показать разбивку» (chkbox_tri/istriang): closed LWPolyline на линии_высот3 (h_triang:131-133)
(defun opor-level-draw-triangles (/ triangles tri coords arr pl n)
  (setq triangles (opor-session-get 'level-triangles))
  (setq n 0)
  (if triangles
    (progn
      (opor-ensure-layer *opor-layer-triangles* 8 "Continuous")
      (foreach tri triangles
        (setq coords
          (list
            (car (cdr (assoc 'a tri))) (cadr (cdr (assoc 'a tri)))
            (car (cdr (assoc 'b tri))) (cadr (cdr (assoc 'b tri)))
            (car (cdr (assoc 'c tri))) (cadr (cdr (assoc 'c tri)))))
        (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 5)))
        (vlax-safearray-fill arr coords)
        (setq pl (vla-AddLightWeightPolyline (opor-ms) arr))
        (vla-put-Closed pl :vlax-true)
        (vla-put-Layer pl *opor-layer-triangles*)
        (if (opor-register-created pl "triangle")
          (setq n (1+ n))))))
  n)

(defun opor-sign2d (p a b)
  (-
    (* (- (car p) (car b)) (- (cadr a) (cadr b)))
    (* (- (car a) (car b)) (- (cadr p) (cadr b)))))

(defun opor-point-in-triangle-p (pt tri / a b c d1 d2 d3 has-neg has-pos tol)
  (setq tol 0.01)
  (setq a (cdr (assoc 'a tri)))
  (setq b (cdr (assoc 'b tri)))
  (setq c (cdr (assoc 'c tri)))
  (setq d1 (opor-sign2d pt a b))
  (setq d2 (opor-sign2d pt b c))
  (setq d3 (opor-sign2d pt c a))
  (setq has-neg (or (< d1 (- tol)) (< d2 (- tol)) (< d3 (- tol))))
  (setq has-pos (or (> d1 tol) (> d2 tol) (> d3 tol)))
  (not (and has-neg has-pos)))

(defun opor-triangle-z-at-point (pt tri / a b c z1 z2 z3 den l1 l2 l3)
  (setq a (cdr (assoc 'a tri)))
  (setq b (cdr (assoc 'b tri)))
  (setq c (cdr (assoc 'c tri)))
  (setq z1 (cdr (assoc 'az tri)))
  (setq z2 (cdr (assoc 'bz tri)))
  (setq z3 (cdr (assoc 'cz tri)))
  (setq den
    (+
      (* (- (cadr b) (cadr c)) (- (car a) (car c)))
      (* (- (car c) (car b)) (- (cadr a) (cadr c)))))
  (if (equal den 0.0 1e-9)
    nil
    (progn
      (setq l1
        (/ (+
             (* (- (cadr b) (cadr c)) (- (car pt) (car c)))
             (* (- (car c) (car b)) (- (cadr pt) (cadr c))))
           den))
      (setq l2
        (/ (+
             (* (- (cadr c) (cadr a)) (- (car pt) (car c)))
             (* (- (car a) (car c)) (- (cadr pt) (cadr c))))
           den))
      (setq l3 (- 1.0 l1 l2))
      (+ (* l1 z1) (* l2 z2) (* l3 z3)))))

(defun opor-variable-hmax (/ tile-mode hmax)
  (setq tile-mode (opor-session-get 'tile-mode))
  (setq hmax
    (if (= tile-mode "d")
      (- (opor-session-get 'floor-height)
         (opor-session-get 'board-thickness)
         (opor-session-get 'lag-thickness))
      (- (opor-session-get 'floor-height)
         (opor-session-get 'tile-thickness)
         (if (opor-floor-height-uses-lag-p)
           (opor-session-get 'lag-thickness)
           0.0))))
  (- hmax (opor-floor-fastener-thickness)))

;; --- Высоты по классам точек ---

;; Вершины (getH_vertecs): lev = hmax - отметка, БЕЗ округления
(defun opor-level-height-for-vertex (pt marks hmax / h)
  (setq h (opor-level-mark-height marks pt))
  (if h (- hmax h) nil))

;; Границы (getH_bords): интерполяция вдоль ближайшего сегмента области
;; (param полилинии внутри сегмента пропорционален длине и для дуг),
;; lev = Round(hmax - h, 0) банковское
(defun opor-level-height-for-border (pt plines marks hmax / best best-dist pline closest dist param endp seg tval p1 p2 h1 h2 h)
  (setq best nil)
  (setq best-dist nil)
  (foreach pline plines
    (setq closest
      (vl-catch-all-apply
        '(lambda () (vlax-curve-getClosestPointTo pline (opor-2d pt)))))
    (if (not (vl-catch-all-error-p closest))
      (progn
        (setq dist (distance (opor-2d pt) (opor-2d closest)))
        (if (and (< dist *opor-vba-border-line-tolerance*)
                 (or (not best-dist) (< dist best-dist)))
          (progn
            (setq best-dist dist)
            (setq best (list pline closest)))))))
  (if (not best)
    nil
    (progn
      (setq pline (car best))
      (setq closest (cadr best))
      (setq param
        (vl-catch-all-apply
          '(lambda () (vlax-curve-getParamAtPoint pline closest))))
      (if (vl-catch-all-error-p param)
        nil
        (progn
          (setq endp (fix (vlax-curve-getEndParam pline)))
          (setq seg (fix param))
          (if (>= seg endp) (setq seg (1- endp)))
          (if (< seg 0) (setq seg 0))
          (setq tval (- param seg))
          (setq p1 (vlax-curve-getPointAtParam pline seg))
          (setq p2 (vlax-curve-getPointAtParam pline (1+ seg)))
          (setq h1 (opor-level-mark-height marks p1))
          (setq h2 (opor-level-mark-height marks p2))
          (if (and h1 h2)
            (progn
              (setq h (+ (* h1 (- 1.0 tval)) (* h2 tval)))
              (opor-round-half-even (- hmax h)))
            nil))))))

;; Узлы (getH_nods): lev = hmax - Round(z, 0) банковское
(defun opor-level-height-for-node (pt triangles hmax / result tri z)
  (setq result nil)
  (foreach tri triangles
    (if (and (not result) (opor-point-in-triangle-p pt tri))
      (progn
        (setq z (opor-triangle-z-at-point pt tri))
        (if z (setq result (- hmax (opor-round-half-even z)))))))
  result)

;; VBA пишет число как есть: целое без дробной части
(defun opor-height-text (value)
  (if (equal value (float (fix value)) 1e-9)
    (itoa (fix value))
    (rtos value 2 2)))

(defun opor-support-count-color-row (support counts / index)
  (setq index (cdr (assoc 'index support)))
  (opor-inc-index-count index counts))

;; Поставить одну опору с расчётной высотой; nil lev = ошибка (цвет 30, "?")
(defun opor-level-place-one (pt lev supports / support color text block)
  (setq support (if lev (opor-support-for-height lev supports) nil))
  (if support
    (progn
      (setq color (cdr (assoc 'color support)))
      (setq text (opor-height-text lev)))
    (progn
      (setq color *opor-error-color*)
      (setq text "?")))
  (setq block (opor-support-insert-with pt color text))
  (list block support))

;; --- Фаза 3: расстановка (b2_mains: вершины -> границы -> узлы) ---
(defun opor-supports-place-variable (session / groups vertices border nodes notborder marks plines triangles hmax supports holes blocks counts errors insert-errors pt lev placed block support)
  (setq marks (opor-session-get 'level-marks))
  (setq plines (opor-session-get 'level-polylines))
  (setq triangles (opor-session-get 'level-triangles))
  (setq holes (opor-session-get 'holes))
  (setq hmax (opor-variable-hmax))
  (setq groups (opor-support-point-groups session))
  (setq vertices (cdr (assoc 'vertices groups)))
  (setq border (cdr (assoc 'border groups)))
  (setq nodes (cdr (assoc 'nodes groups)))
  (setq supports (opor-read-supports (opor-session-get 'line)))
  (setq supports (opor-infer-support-colors (opor-session-get 'line) supports))
  (opor-session-set 'support-ranges supports)
  (setq blocks '())
  (setq counts '())
  (setq errors 0)
  (setq insert-errors 0)
  (opor-session-set 'var-aborted nil)
  ;; вершины: отметка прямо в точке
  (foreach pt vertices
    (setq lev (opor-level-height-for-vertex pt marks hmax))
    (setq placed (opor-level-place-one pt lev supports))
    (setq block (car placed))
    (setq support (cadr placed))
    (if block
      (progn
        (setq blocks (cons block blocks))
        (if support
          (setq counts (opor-support-count-color-row support counts))
          (setq errors (1+ errors))))
      (setq insert-errors (1+ insert-errors))))
  ;; границы: интерполяция вдоль линий областей высот
  (setq notborder '())
  (foreach pt border
    (setq lev (opor-level-height-for-border pt plines marks hmax))
    (if lev
      (progn
        (setq placed (opor-level-place-one pt lev supports))
        (setq block (car placed))
        (setq support (cadr placed))
        (if block
          (progn
            (setq blocks (cons block blocks))
            (if support
              (setq counts (opor-support-count-color-row support counts))
              (setq errors (1+ errors))))
          (setq insert-errors (1+ insert-errors))))
      (setq notborder (cons pt notborder))))
  (setq notborder (reverse notborder))
  (if (and notborder (not holes))
    ;; VBA: MsgBox + выход, таблица не вставляется, круги цвета 30
    (progn
      (opor-error-circles notborder)
      (opor-alert
        (strcat
          "Есть точки, находящиеся не на границе областей высот.\n"
          "Точки помечены окружностями оранжевого цвета: "
          (itoa (length notborder))))
      (opor-session-set 'var-aborted T)
      (setq blocks (reverse blocks))
      (opor-session-set 'support-blocks blocks)
      (opor-session-set 'support-count (length blocks))
      (opor-session-set 'support-counts counts)
      (opor-session-set 'support-height-errors errors)
      (opor-session-set 'support-insert-errors insert-errors)
      blocks)
    (progn
      ;; VBA: точки "не на границе" уходят в узлы
      (setq nodes (append nodes notborder))
      (foreach pt nodes
        (setq lev (opor-level-height-for-node pt triangles hmax))
        (setq placed (opor-level-place-one pt lev supports))
        (setq block (car placed))
        (setq support (cadr placed))
        (if block
          (progn
            (setq blocks (cons block blocks))
            (if support
              (setq counts (opor-support-count-color-row support counts))
              (setq errors (1+ errors))))
          (setq insert-errors (1+ insert-errors))))
      (setq blocks (reverse blocks))
      (opor-session-set 'support-blocks blocks)
      (opor-session-set 'support-count (length blocks))
      (opor-session-set 'support-counts counts)
      (opor-session-set 'support-height-errors errors)
      (opor-session-set 'support-insert-errors insert-errors)
      blocks)))

(princ)
