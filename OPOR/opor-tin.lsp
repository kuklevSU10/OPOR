;;; OPOR TIN: препроцессор областей высот по блокам отметок (Делоне V1).

(setq *opor-tin-pending-objects* nil)

(defun opor-tin-track-pending-object (obj)
  (if obj
    (setq *opor-tin-pending-objects*
      (cons obj *opor-tin-pending-objects*)))
  obj)

(defun opor-tin-clear-pending-objects ()
  (setq *opor-tin-pending-objects* nil)
  (princ))

(defun opor-tin-rollback-pending-objects (/ obj)
  (foreach obj *opor-tin-pending-objects*
    (if (opor-object-live-p obj)
      (if (opor-delete-object obj)
        (opor-unregister-created obj))))
  (setq *opor-tin-pending-objects* nil)
  (princ))

(defun opor-tin-point-equal-p (a b)
  (equal (opor-2d a) (opor-2d b) 1e-8))

(defun opor-tin-point-less-p (a b)
  (or
    (< (car a) (car b))
    (and (equal (car a) (car b) 1e-9)
         (< (cadr a) (cadr b)))))

(defun opor-tin-orient2d (a b c)
  (-
    (* (- (car b) (car a)) (- (cadr c) (cadr a)))
    (* (- (cadr b) (cadr a)) (- (car c) (car a)))))

(defun opor-tin-circumcircle-contains-p (tri p / a b c ax ay bx by cx cy a2 b2 c2 d ux uy r2 p2 tol)
  (setq a (car tri) b (cadr tri) c (caddr tri))
  (setq ax (car a) ay (cadr a))
  (setq bx (car b) by (cadr b))
  (setq cx (car c) cy (cadr c))
  (setq d
    (* 2.0
      (+ (* ax (- by cy))
         (* bx (- cy ay))
         (* cx (- ay by)))))
  (if (equal d 0.0 1e-12)
    nil
    (progn
      (setq a2 (+ (* ax ax) (* ay ay)))
      (setq b2 (+ (* bx bx) (* by by)))
      (setq c2 (+ (* cx cx) (* cy cy)))
      (setq ux
        (/ (+ (* a2 (- by cy))
              (* b2 (- cy ay))
              (* c2 (- ay by)))
           d))
      (setq uy
        (/ (+ (* a2 (- cx bx))
              (* b2 (- ax cx))
              (* c2 (- bx ax)))
           d))
      (setq r2 (+ (* (- ax ux) (- ax ux)) (* (- ay uy) (- ay uy))))
      (setq p2
        (+ (* (- (car p) ux) (- (car p) ux))
           (* (- (cadr p) uy) (- (cadr p) uy))))
      (setq tol (* 1e-9 (max 1.0 r2)))
      (<= p2 (+ r2 tol)))))

(defun opor-tin-edge-equal-p (a b)
  (or
    (and (opor-tin-point-equal-p (car a) (car b))
         (opor-tin-point-equal-p (cadr a) (cadr b)))
    (and (opor-tin-point-equal-p (car a) (cadr b))
         (opor-tin-point-equal-p (cadr a) (car b)))))

;; XOR рёбер: внутреннее ребро двух плохих треугольников исчезает,
;; внешняя граница полости остаётся один раз.
(defun opor-tin-toggle-edge (edge edges / item result removed)
  (setq result '() removed nil)
  (foreach item edges
    (if (and (not removed) (opor-tin-edge-equal-p edge item))
      (setq removed T)
      (setq result (cons item result))))
  (setq result (reverse result))
  (if removed result (cons edge result)))

(defun opor-tin-triangle-edges (tri)
  (list
    (list (car tri) (cadr tri))
    (list (cadr tri) (caddr tri))
    (list (caddr tri) (car tri))))

(defun opor-tin-triangle-has-point-p (tri pt)
  (vl-some '(lambda (item) (opor-tin-point-equal-p item pt)) tri))

(defun opor-tin-super-triangle (points / xs ys minx maxx miny maxy dx dy delta mx my)
  (setq xs (mapcar 'car points))
  (setq ys (mapcar 'cadr points))
  (setq minx (apply 'min xs) maxx (apply 'max xs))
  (setq miny (apply 'min ys) maxy (apply 'max ys))
  (setq dx (- maxx minx) dy (- maxy miny))
  (setq delta (max dx dy 1.0))
  (setq mx (/ (+ minx maxx) 2.0))
  (setq my (/ (+ miny maxy) 2.0))
  (list
    (list (- mx (* 20.0 delta)) (- my delta) 0.0)
    (list mx (+ my (* 20.0 delta)) 0.0)
    (list (+ mx (* 20.0 delta)) (- my delta) 0.0)))

(defun opor-tin-delaunay (points / super triangles point tri bad polygon edge kept result)
  (setq points (vl-sort points 'opor-tin-point-less-p))
  (setq super (opor-tin-super-triangle points))
  (setq triangles (list super))
  (foreach point points
    (setq bad '())
    (foreach tri triangles
      (if (opor-tin-circumcircle-contains-p tri point)
        (setq bad (cons tri bad))))
    (setq polygon '())
    (foreach tri bad
      (foreach edge (opor-tin-triangle-edges tri)
        (setq polygon (opor-tin-toggle-edge edge polygon))))
    (setq kept '())
    (foreach tri triangles
      (if (not (member tri bad))
        (setq kept (cons tri kept))))
    (setq triangles (reverse kept))
    (foreach edge polygon
      (if (> (abs (opor-tin-orient2d (car edge) (cadr edge) point)) 1e-8)
        (setq triangles (cons (list (car edge) (cadr edge) point) triangles)))))
  (setq result '())
  (foreach tri triangles
    (if (and
          (not (vl-some '(lambda (sp) (opor-tin-triangle-has-point-p tri sp)) super))
          (> (opor-triangle-area2d (car tri) (cadr tri) (caddr tri)) 1e-6))
      (setq result (cons tri result))))
  (reverse result))

;; --- Восстановление рёбер контура (constrained Delaunay) ---

(defun opor-tin-edge-in-triangle-p (edge tri)
  (vl-some
    '(lambda (item) (opor-tin-edge-equal-p edge item))
    (opor-tin-triangle-edges tri)))

(defun opor-tin-edge-exists-p (edge triangles)
  (vl-some
    '(lambda (tri) (opor-tin-edge-in-triangle-p edge tri))
    triangles))

(defun opor-tin-unique-edges (triangles / result tri edge)
  (setq result '())
  (foreach tri triangles
    (foreach edge (opor-tin-triangle-edges tri)
      (if (not (vl-some '(lambda (item) (opor-tin-edge-equal-p edge item)) result))
        (setq result (cons edge result)))))
  (reverse result))

(defun opor-tin-proper-edge-intersection-p (a b c d / o1 o2 o3 o4)
  (if (or (opor-tin-point-equal-p a c) (opor-tin-point-equal-p a d)
          (opor-tin-point-equal-p b c) (opor-tin-point-equal-p b d))
    nil
    (progn
      (setq o1 (opor-tin-orient2d a b c))
      (setq o2 (opor-tin-orient2d a b d))
      (setq o3 (opor-tin-orient2d c d a))
      (setq o4 (opor-tin-orient2d c d b))
      (and (< (* o1 o2) 0.0) (< (* o3 o4) 0.0)))))

(defun opor-tin-triangles-with-edge (edge triangles / result tri)
  (setq result '())
  (foreach tri triangles
    (if (opor-tin-edge-in-triangle-p edge tri)
      (setq result (cons tri result))))
  (reverse result))

(defun opor-tin-opposite-point (tri edge / found point)
  (setq found nil)
  (foreach point tri
    (if (and (not found)
             (not (opor-tin-point-equal-p point (car edge)))
             (not (opor-tin-point-equal-p point (cadr edge))))
      (setq found point)))
  found)

(defun opor-tin-edge-flip-data (edge triangles / adjacent c d e f new-edge)
  (setq adjacent (opor-tin-triangles-with-edge edge triangles))
  (if (= (length adjacent) 2)
    (progn
      (setq c (car edge) d (cadr edge))
      (setq e (opor-tin-opposite-point (car adjacent) edge))
      (setq f (opor-tin-opposite-point (cadr adjacent) edge))
      (setq new-edge (list e f))
      (if (and e f
               (not (opor-tin-point-equal-p e f))
               (< (* (opor-tin-orient2d e f c)
                     (opor-tin-orient2d e f d)) 0.0)
               (not (opor-tin-edge-exists-p new-edge triangles)))
        (list adjacent c d e f new-edge)
        nil))
    nil))

(defun opor-tin-flip-edge (edge triangles / info adjacent c d e f kept tri)
  (setq info (opor-tin-edge-flip-data edge triangles))
  (if (not info)
    nil
    (progn
      (setq adjacent (nth 0 info) c (nth 1 info) d (nth 2 info)
            e (nth 3 info) f (nth 4 info) kept '())
      (foreach tri triangles
        (if (not (member tri adjacent)) (setq kept (cons tri kept))))
      (cons (list e f c) (cons (list f e d) (reverse kept))))))

(defun opor-tin-crossing-edges (constraint triangles / result edge)
  (setq result '())
  (foreach edge (opor-tin-unique-edges triangles)
    (if (opor-tin-proper-edge-intersection-p
          (car constraint) (cadr constraint) (car edge) (cadr edge))
      (setq result (cons edge result))))
  (reverse result))

(defun opor-tin-remove-edge (target edges / result edge)
  (setq result '())
  (foreach edge edges
    (if (not (opor-tin-edge-equal-p target edge))
      (setq result (cons edge result))))
  (reverse result))

;; Очередь переворотов нужна для длинных вогнутых рёбер. Иногда единственный
;; доступный первый flip ещё пересекает constraint, но делает выпуклыми соседние
;; четырёхугольники. Старый код запрещал такой промежуточный шаг и останавливался.
(defun opor-tin-recover-edge (constraint triangles / limit flips done queue edge info value new-edge misses cycle)
  (setq limit (* 50 (max 1 (length triangles))))
  (setq flips 0 done (opor-tin-edge-exists-p constraint triangles))
  (setq queue (opor-tin-crossing-edges constraint triangles))
  (setq misses 0 cycle (max 1 (length queue)))
  (while (and (not done) queue (< flips limit) (< misses cycle))
    (setq edge (car queue) queue (cdr queue))
    (if (and (opor-tin-edge-exists-p edge triangles)
             (opor-tin-proper-edge-intersection-p
               (car constraint) (cadr constraint) (car edge) (cadr edge)))
      (progn
        (setq info (opor-tin-edge-flip-data edge triangles))
        (if info
          (progn
            (setq value (opor-tin-flip-edge edge triangles))
            (setq triangles value flips (1+ flips) misses 0)
            (setq done (opor-tin-edge-exists-p constraint triangles))
            (if (not done)
              (progn
                (setq new-edge (nth 5 info))
                (setq queue (opor-tin-crossing-edges constraint triangles))
                ;; Новый диагональный edge ставим в хвост: сначала соседние
                ;; пересечения получают шанс стать переворачиваемыми.
                (setq queue (opor-tin-remove-edge new-edge queue))
                (if (opor-tin-proper-edge-intersection-p
                      (car constraint) (cadr constraint)
                      (car new-edge) (cadr new-edge))
                  (setq queue (append queue (list new-edge))))
                (setq cycle (max 1 (length queue))))))
          (progn
            (setq queue (append queue (list edge)))
            (setq misses (1+ misses)))))
      (setq misses (1+ misses))))
  (list triangles flips done))

(defun opor-tin-point-on-segment-p (point a b tol)
  (<=
    (abs
      (- (+ (distance a point) (distance point b)) (distance a b)))
    tol))

(defun opor-tin-split-constraint (edge points / a b on point ordered result)
  (setq a (car edge) b (cadr edge) on '())
  (foreach point points
    (if (opor-tin-point-on-segment-p point a b 1e-6)
      (setq on (cons point on))))
  (setq ordered
    (vl-sort on '(lambda (p q) (< (distance a p) (distance a q)))))
  (setq result '())
  (while (cdr ordered)
    (if (not (opor-tin-point-equal-p (car ordered) (cadr ordered)))
      (setq result (cons (list (car ordered) (cadr ordered)) result)))
    (setq ordered (cdr ordered)))
  (reverse result))

(defun opor-tin-ring-constraints (ring points / result prev point)
  (setq result '())
  (if ring
    (progn
      (setq prev (car (reverse ring)))
      (foreach point ring
        (setq result
          (append result (opor-tin-split-constraint (list prev point) points)))
        (setq prev point))))
  result)

(defun opor-tin-edge-shared-by-boundary-and-hole-p
  (edge boundary holes / a b mid found hole)
  (setq a (car edge) b (cadr edge))
  (setq mid
    (list
      (/ (+ (car a) (car b)) 2.0)
      (/ (+ (cadr a) (cadr b)) 2.0)
      0.0))
  (setq found nil)
  (if (and
        (opor-point-on-curve-p a boundary *opor-point-tolerance*)
        (opor-point-on-curve-p mid boundary *opor-point-tolerance*)
        (opor-point-on-curve-p b boundary *opor-point-tolerance*))
    (foreach hole holes
      (if (and
            (not found)
            (opor-point-on-curve-p a hole *opor-point-tolerance*)
            (opor-point-on-curve-p mid hole *opor-point-tolerance*)
            (opor-point-on-curve-p b hole *opor-point-tolerance*))
        (setq found T))))
  found)

;; Если проём примыкает к внешнему контуру общей линией, по обе стороны этого
;; отрезка нет рабочей области: снаружи находится внешний мир, изнутри — проём.
;; Такое ребро не обязано присутствовать в TIN и на коллинеарной границе может
;; быть невосстановимо переворотами Делоне. Остальные рёбра обоих колец остаются
;; обязательными constraints.
(defun opor-tin-remove-shared-boundary-constraints
  (constraints boundary holes / result skipped edge)
  (setq result '() skipped 0)
  (foreach edge constraints
    (if (opor-tin-edge-shared-by-boundary-and-hole-p edge boundary holes)
      (setq skipped (1+ skipped))
      (setq result (cons edge result))))
  (if (and (boundp '*opor-session*) *opor-session*)
    (opor-session-set 'tin-shared-boundary-constraint-count skipped))
  (if (> skipped 0)
    (opor-log
      (strcat
        "TIN: исключено общих рёбер внешнего контура и проёмов="
        (itoa skipped) ".")))
  (reverse result))

(defun opor-tin-build-constraints (boundary holes points / result hole)
  (setq result
    (opor-tin-ring-constraints
      (opor-polyline-linearized-vertices
        boundary *opor-curve-chord-tolerance*)
      points))
  (foreach hole holes
    (setq result
      (append result
        (opor-tin-ring-constraints
          (opor-polyline-linearized-vertices
            hole *opor-curve-chord-tolerance*)
          points))))
  (opor-tin-remove-shared-boundary-constraints result boundary holes))

(defun opor-tin-apply-constraints (triangles constraints / flips failures edge recovered)
  (setq flips 0 failures '())
  (foreach edge constraints
    (setq recovered (opor-tin-recover-edge edge triangles))
    (setq triangles (car recovered))
    (setq flips (+ flips (cadr recovered)))
    (if (not (caddr recovered))
      (setq failures (cons edge failures))))
  (list triangles flips (reverse failures)))

(defun opor-tin-show-failed-constraints (failures / points edge a b)
  (setq points '())
  (foreach edge failures
    (setq a (car edge) b (cadr edge))
    (setq points (cons a (cons b points)))
    (opor-log
      (strcat
        "TIN constraint failure: X1=" (rtos (car a) 2 3)
        ", Y1=" (rtos (cadr a) 2 3)
        ", X2=" (rtos (car b) 2 3)
        ", Y2=" (rtos (cadr b) 2 3) ".")))
  (setq points
    (opor-unique-points points *opor-vba-point-dedupe-tolerance*))
  (if points
    (progn
      (opor-error-circles points)
      (opor-zoom-to-points points)))
  points)

(defun opor-tin-centroid (tri)
  (list
    (/ (+ (car (car tri)) (car (cadr tri)) (car (caddr tri))) 3.0)
    (/ (+ (cadr (car tri)) (cadr (cadr tri)) (cadr (caddr tri))) 3.0)
    0.0))

(defun opor-tin-filter-triangles (triangles boundary holes / result tri)
  (setq result '())
  (foreach tri triangles
    (if (opor-point-in-working-area-p (opor-tin-centroid tri) boundary holes)
      (setq result (cons tri result))))
  (reverse result))

(defun opor-tin-find-near-mark (mark marks / found pt other)
  (setq found nil pt (cdr (assoc 'point mark)))
  (foreach other marks
    (if (and (not found)
             (< (distance pt (cdr (assoc 'point other))) *opor-vba-mark-match-tolerance*))
      (setq found other)))
  found)

(defun opor-tin-prepare-marks (marks boundary holes / used conflicts mark old result)
  (setq used '() conflicts '() result '())
  (foreach mark marks
    (if (opor-point-in-working-area-p (cdr (assoc 'point mark)) boundary holes)
      (progn
        (setq old (opor-tin-find-near-mark mark used))
        (cond
          ((not old)
            (setq used (cons mark used))
            (setq result (cons mark result)))
          ((not (equal (cdr (assoc 'height old)) (cdr (assoc 'height mark)) 1e-9))
            (setq conflicts (cons (cdr (assoc 'point mark)) conflicts)))))))
  (list (reverse result) (reverse conflicts)))

;; VBA-допуск сопоставления отметки с вершиной равен 1 мм. Если
;; после такого сопоставления оставить неточную координату в TIN,
;; constraint-ребро может не найти точку. Привязываем только уже
;; принятые внешние отметки, сохраняя высоту и сам блок.
(defun opor-tin-snap-marks-to-points (marks target-points tol / result mark pt target snapped)
  (setq result '())
  (foreach mark marks
    (setq pt (cdr (assoc 'point mark)) snapped nil)
    (foreach target target-points
      (if (and (not snapped)
               (< (abs (- (car pt) (car target))) tol)
               (< (abs (- (cadr pt) (cadr target))) tol))
        (setq snapped target)))
    (if snapped
      (setq mark (subst (cons 'point (opor-2d snapped)) (assoc 'point mark) mark)))
    (setq result (cons mark result)))
  (reverse result))

;; Отметка может находиться в пределах общего геометрического допуска от
;; внешней границы, но оставаться на доли микрона снаружи. Для проверки области
;; это допустимо, а Делоне тогда строит convex hull через отметку и длинное
;; граничное constraint-ребро уже невозможно восстановить переворотами.
;; Проецируем только практически совпадающие с внешней кривой отметки; высота,
;; блок-источник и все остальные свойства сохраняются.
(defun opor-tin-project-near-marks-to-boundary
  (marks boundary tol / result mark pt closest delta projected)
  (setq result '() projected 0)
  (foreach mark marks
    (setq pt (cdr (assoc 'point mark)))
    (setq closest
      (vl-catch-all-apply
        '(lambda ()
          (vlax-curve-getClosestPointTo boundary (opor-2d pt)))))
    (if (and
          (not (vl-catch-all-error-p closest))
          (setq delta (distance (opor-2d pt) (opor-2d closest)))
          (<= delta tol))
      (progn
        (if (> delta 1e-9) (setq projected (1+ projected)))
        (setq mark
          (subst
            (cons 'point (opor-2d closest))
            (assoc 'point mark)
            mark))))
    (setq result (cons mark result)))
  (if (and (boundp '*opor-session*) *opor-session*)
    (opor-session-set 'tin-boundary-projected-mark-count projected))
  (if (> projected 0)
    (opor-log
      (strcat
        "TIN: исходных отметок привязано к точной внешней границе="
        (itoa projected) ".")))
  (reverse result))

;; Отметки, рассчитанные предыдущим запуском TIN, не участвуют как исходные:
;; при повторном запуске их высота должна быть пересчитана по пользовательским
;; отметкам, а не сохранена как устаревшее значение.
(defun opor-tin-generated-mark-p (mark / obj)
  (setq obj (cdr (assoc 'object mark)))
  (and obj
       (member
         (opor-object-xdata-type obj)
         '("tin-interpolated-mark" "tin-interpolated-curve-mark"))))

(defun opor-tin-source-marks (marks)
  (vl-remove-if 'opor-tin-generated-mark-p marks))

(defun opor-tin-old-generated-marks (marks boundary / result mark pt)
  (setq result '())
  (foreach mark marks
    (if (opor-tin-generated-mark-p mark)
      (progn
        (setq pt (cdr (assoc 'point mark)))
        (if (or (opor-point-inside-boundary-p pt boundary)
                (opor-point-on-curve-p pt boundary *opor-point-tolerance*))
          (setq result (cons mark result))))))
  (reverse result))

(defun opor-tin-height-triangle (tri marks / a b c az bz cz)
  (setq a (car tri) b (cadr tri) c (caddr tri))
  (setq az (opor-level-mark-height marks a))
  (setq bz (opor-level-mark-height marks b))
  (setq cz (opor-level-mark-height marks c))
  (if (and (numberp az) (numberp bz) (numberp cz))
    (list
      (cons 'a a) (cons 'az az)
      (cons 'b b) (cons 'bz bz)
      (cons 'c c) (cons 'cz cz))
    nil))

;; Линейная интерполяция высоты вершины проёма по предварительной поверхности,
;; построенной только из реальных пользовательских отметок.
(defun opor-tin-interpolate-height (pt triangles marks / result tri height-tri value)
  (setq result nil)
  (foreach tri triangles
    (if (not (numberp result))
      (progn
        (setq height-tri (opor-tin-height-triangle tri marks))
        (if (and height-tri (opor-point-in-triangle-p pt height-tri))
          (progn
            (setq value (opor-triangle-z-at-point pt height-tri))
            (if (numberp value) (setq result value)))))))
  result)

(defun opor-tin-interpolate-hole-marks (points triangles marks / generated unresolved pt height)
  (setq generated '() unresolved '())
  (foreach pt points
    (setq height (opor-tin-interpolate-height pt triangles marks))
    (if (numberp height)
      (setq generated
        (cons
          (list
            (cons 'point (opor-2d pt))
            (cons 'height height)
            (cons 'color 256)
            (cons 'generated T))
          generated))
      (setq unresolved (cons pt unresolved))))
  (list (reverse generated) (reverse unresolved)))

;; Второй безопасный способ получить отметку вершины проёма, если
;; предварительный TIN её не покрывает: для точки на внешней границе берём две
;; исходные отметки концов того же сегмента и линейно интерполируем высоту по
;; параметру полилинии. Для внутренней точки ничего не угадываем — она остаётся
;; для ручного ввода.
(defun opor-tin-interpolate-boundary-hole-marks
  (points boundary marks / generated unresolved pt closest entry)
  (setq generated '() unresolved '())
  (foreach pt points
    (setq entry nil)
    (if (opor-point-on-curve-p pt boundary *opor-point-tolerance*)
      (progn
        (setq closest
          (vl-catch-all-apply
            '(lambda ()
              (vlax-curve-getClosestPointTo boundary (opor-2d pt)))))
        (if (not (vl-catch-all-error-p closest))
          (progn
            (setq entry
              (opor-level-curve-entry-at-point boundary closest marks))
            (if entry
              (setq entry
                (subst
                  (cons 'point (opor-2d pt))
                  (assoc 'point entry)
                  entry)))))))
    (if entry
      (setq generated
        (cons
          (append entry
            (list
              (cons 'generated T)
              (cons 'boundary-derived T)))
          generated))
      (setq unresolved (cons pt unresolved))))
  (list (reverse generated) (reverse unresolved)))

(defun opor-tin-prompt-hole-mark-mode (points / answer)
  (opor-alert
    (strcat
      "Предварительная поверхность не покрыла вершины проёмов: "
      (itoa (length points)) ".\n\n"
      "Авто — рассчитать отметки граничных вершин по соседним исходным "
      "отметкам внешнего контура.\n"
      "Вручную — ввести значения по очереди.\n"
      "Отмена — остановить TIN без изменений."))
  (initget "Авто Вручную Отмена")
  (setq answer
    (getkword
      "\nОбработать проблемные вершины [Авто/Вручную/Отмена] <Авто>: "))
  (if answer answer "Авто"))

(defun opor-tin-edge-point-at-ratio (edge ratio / a b)
  (setq a (car edge) b (cadr edge))
  (list
    (+ (car a) (* ratio (- (car b) (car a))))
    (+ (cadr a) (* ratio (- (cadr b) (cadr a))))
    0.0))

;; Адаптивный retry для ограничения, которое flip-алгоритм не смог вернуть
;; целиком. Только прямой отрезок настоящей внешней границы получает несколько
;; технических точек; их высоты берутся с того же сегмента контура. После этого
;; Делоне и constraints строятся заново по коротким частям. Точки помечаются как
;; curve-sample, поэтому следующий Var использует их высоты, но не ставит в них
;; обязательные конструктивные опоры.
(defun opor-tin-failed-boundary-constraint-marks
  (failures boundary marks / result edge a b mid edge-length max-length pieces index ratio pt closest entry)
  (setq result '())
  (setq max-length
    (if (and
          (boundp '*opor-tin-retry-constraint-length*)
          (numberp *opor-tin-retry-constraint-length*)
          (> *opor-tin-retry-constraint-length* 0.0))
      *opor-tin-retry-constraint-length*
      4000.0))
  (foreach edge failures
    (setq a (car edge) b (cadr edge))
    (setq mid (opor-tin-edge-point-at-ratio edge 0.5))
    (if (and
          (opor-point-on-curve-p a boundary *opor-point-tolerance*)
          (opor-point-on-curve-p mid boundary *opor-point-tolerance*)
          (opor-point-on-curve-p b boundary *opor-point-tolerance*))
      (progn
        (setq edge-length (distance a b))
        (setq pieces
          (max 2 (opor-ceiling-positive (/ edge-length max-length))))
        (setq index 1)
        (while (< index pieces)
          (setq ratio (/ (float index) pieces))
          (setq pt (opor-tin-edge-point-at-ratio edge ratio))
          (setq closest
            (vl-catch-all-apply
              '(lambda ()
                (vlax-curve-getClosestPointTo boundary (opor-2d pt)))))
          (setq entry nil)
          (if (not (vl-catch-all-error-p closest))
            (progn
              (setq entry
                (opor-level-curve-entry-at-point boundary closest marks))
              (if entry
                (setq entry
                  (subst
                    (cons 'point (opor-2d closest))
                    (assoc 'point entry)
                    entry)))))
          (if (and
                entry
                (not (opor-tin-exact-mark-at-point-p marks (cdr (assoc 'point entry))))
                (not (opor-tin-exact-mark-at-point-p result (cdr (assoc 'point entry)))))
            (setq result
              (cons
                (append entry
                  (list
                    (cons 'curve-sample T)
                    (cons 'constraint-sample T)))
                result)))
          (setq index (1+ index))))))
  (reverse result))

(defun opor-tin-exact-mark-at-point-p (marks pt / found mark)
  (setq found nil)
  (foreach mark marks
    (if (and (not found)
             (opor-tin-point-equal-p (cdr (assoc 'point mark)) pt))
      (setq found T)))
  found)

;; TIN умеет восстанавливать только прямые constraint-рёбра. Поэтому настоящая
;; дуга представляется хордами с заданной стрелой, а их концы добавляются во
;; внутренний набор отметок. Высота идёт по длине дугового сегмента между
;; пользовательскими отметками его концов. На этапе расчёта это виртуальные
;; отметки; после успешной сборки TIN они сохраняются блоками, чтобы следующий
;; Var нашёл отметку в каждой вершине созданных треугольников.
(defun opor-tin-curve-sample-marks (curves marks / result curve points pt entry)
  (setq result '())
  (foreach curve curves
    (if (opor-polyline-has-arc-segments-p curve)
      (progn
        (setq points
          (opor-polyline-linearized-vertices
            curve *opor-curve-chord-tolerance*))
        (foreach pt points
          (if (and (not (opor-tin-exact-mark-at-point-p marks pt))
                   (not (opor-tin-exact-mark-at-point-p result pt)))
            (progn
              (setq entry
                (opor-level-curve-entry-at-point curve pt marks))
              (if entry
                (setq result
                  (cons
                    (append entry (list (cons 'curve-sample T)))
                    result))))))))
  (reverse result)))

(defun opor-tin-manual-hole-mark (pt height)
  (list
    (cons 'point (opor-2d pt))
    (cons 'height height)
    (cons 'color 256)
    (cons 'manual T)))

(defun opor-tin-mark-template (marks / found mark)
  (setq found nil)
  (foreach mark marks
    (if (and (not found) (/= (cdr (assoc 'color mark)) 1))
      (setq found (cdr (assoc 'object mark)))))
  (if found found (cdr (assoc 'object (car marks)))))

(defun opor-tin-format-height (height)
  ;; 0.001 мм достаточно для последующего CLng, но не перегружает подпись.
  (rtos height 2 3))

(defun opor-tin-set-mark-text (block text / raw atts result)
  (setq result nil)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (opor-variant-list raw))
      (if atts
        (progn
          (setq raw
            (vl-catch-all-apply 'vla-put-TextString (list (car atts) text)))
          (if (not (vl-catch-all-error-p raw)) (setq result T))))))
  result)

(defun opor-tin-insert-generated-mark (mark template / pt sx sy sz rotation layer color object-type value block)
  (setq pt (cdr (assoc 'point mark)))
  (setq sx (if template (vla-get-XScaleFactor template) 1.0))
  (setq sy (if template (vla-get-YScaleFactor template) 1.0))
  (setq sz (if template (vla-get-ZScaleFactor template) 1.0))
  (setq rotation (if template (vla-get-Rotation template) 0.0))
  (setq layer (if template (vla-get-Layer template) "0"))
  (setq color (if template (vla-get-Color template) 256))
  (if (= color 1) (setq color 256))
  (setq object-type
    (cond
      ((cdr (assoc 'manual mark)) "tin-manual-hole-mark")
      ((cdr (assoc 'curve-sample mark)) "tin-interpolated-curve-mark")
      (t "tin-interpolated-mark")))
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (opor-ms) (vlax-3d-point pt) *opor-level-block-name*
        sx sy sz rotation)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq block value)
      (vl-catch-all-apply 'vla-put-Layer (list block layer))
      (vl-catch-all-apply 'vla-put-Color (list block color))
      (if (opor-tin-set-mark-text
            block (opor-tin-format-height (cdr (assoc 'height mark))))
        (progn
          (setq block (opor-register-created block object-type))
          (if (and block (= (opor-object-xdata-type block) object-type))
            (opor-tin-track-pending-object block)
            (progn
              (opor-unregister-created block)
              (opor-delete-object block)
              nil)))
        (progn
          (opor-delete-object block)
          nil)))))

(defun opor-tin-insert-generated-marks (marks source-marks / template blocks failures mark block)
  (setq template (opor-tin-mark-template source-marks))
  (setq blocks '() failures 0)
  (foreach mark marks
    (setq block (opor-tin-insert-generated-mark mark template))
    (if block
      (setq blocks (cons block blocks))
      (setq failures (1+ failures))))
  (list (reverse blocks) failures))

(defun opor-tin-zoom-to-unresolved-point (pt / radius)
  (setq radius (* 4.0 *opor-error-circle-radius*))
  (vl-catch-all-apply
    'vla-ZoomWindow
    (list
      (vlax-get-acad-object)
      (vlax-3d-point
        (list (- (car pt) radius) (- (cadr pt) radius) 0.0))
      (vlax-3d-point
        (list (+ (car pt) radius) (+ (cadr pt) radius) 0.0)))))

;; Если предварительный TIN не покрывает вершину проёма, не экстраполируем
;; высоту молча. Показываем точку и даём пользователю явно задать её отметку.
;; Такие блоки остаются исходными при повторном TIN, но чистятся OPORCLEAN.
(defun opor-tin-prompt-manual-hole-marks
  (points boundary source-marks / markers marker answer template result blocks failed
                                index count pt input height mark block)
  (setq markers '())
  (foreach pt points
    (setq marker (opor-error-circle pt))
    (if marker (setq markers (cons marker markers))))
  (opor-alert
    (strcat
      "Не удалось автоматически вычислить отметки "
      (itoa (length points)) " вершин проёмов.\n"
      "Точки отмечены оранжевыми кругами.\n\n"
      "После закрытия окна можно ввести отметку каждой вершины, "
      "и OPOR продолжит построение TIN."))
  (initget "Да Нет")
  (setq answer
    (getkword "\nВвести отметки проблемных вершин вручную? [Да/Нет] <Да>: "))
  (if (= answer "Нет")
    (progn
      (opor-log
        (strcat "TIN отменён: ручной ввод отметок вершин проёмов отклонён, точек="
          (itoa (length points)) "."))
      (list nil T))
    (progn
      (setq template (opor-tin-mark-template source-marks))
      (setq result '() blocks '() failed nil index 1 count (length points))
      (foreach pt points
        (if (not failed)
          (progn
            (opor-tin-zoom-to-unresolved-point pt)
            (initget 1)
            (setq input
              (vl-catch-all-apply
                'getreal
                (list
                  (strcat
                    "\nОтметка вершины проёма " (itoa index) " из " (itoa count)
                    " (X=" (rtos (car pt) 2 3)
                    ", Y=" (rtos (cadr pt) 2 3) "), мм: "))))
            (if (or (vl-catch-all-error-p input) (not (numberp input)))
              (setq failed T)
              (progn
                (setq height input)
                (setq mark (opor-tin-manual-hole-mark pt height))
                (setq block (opor-tin-insert-generated-mark mark template))
                (if block
                  (progn
                    (setq mark (append mark (list (cons 'object block))))
                    (setq result (cons mark result))
                    (setq blocks (cons block blocks)))
                  (setq failed T))))
            (setq index (1+ index)))))
      (if failed
        (progn
          (opor-tin-delete-blocks blocks)
          (opor-zoom-to-boundary boundary)
          (opor-alert
            "Ручной ввод отметок отменён или блок отметки создать не удалось. TIN не построен.")
          (list nil T))
        (progn
          (opor-tin-delete-blocks markers)
          (opor-zoom-to-boundary boundary)
          (setq result (reverse result))
          (opor-log
            (strcat "TIN: вручную заданы отметки вершин проёмов="
              (itoa (length result)) "."))
          (list result nil))))))

(defun opor-tin-delete-blocks (blocks / count block)
  (setq count 0)
  (foreach block blocks
    (if (opor-object-live-p block)
      (if (opor-delete-object block)
        (progn
         (opor-unregister-created block)
          (setq count (1+ count))))))
  count)

(defun opor-tin-mark-blocks (marks / result mark block)
  (setq result '())
  (foreach mark marks
    (setq block (cdr (assoc 'object mark)))
    (if block (setq result (cons block result))))
  (reverse result))

(defun opor-tin-rollback-new-objects (triangles slopes generated manual)
  ;; Аргументы оставлены для читаемости места вызова; журнал pending
  ;; содержит все новые объекты, включая созданные до неожиданной ошибки.
  (opor-tin-rollback-pending-objects)
  (princ))

(defun opor-tin-delete-old-generated-marks (marks / blocks mark obj)
  (setq blocks '())
  (foreach mark marks
    (setq obj (cdr (assoc 'object mark)))
    (if obj (setq blocks (cons obj blocks))))
  (opor-tin-delete-blocks blocks))

(defun opor-tin-create-polyline (tri / coords arr value pl closed-result layer-result registered)
  (setq coords
    (list
      (car (car tri)) (cadr (car tri))
      (car (cadr tri)) (cadr (cadr tri))
      (car (caddr tri)) (cadr (caddr tri))))
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill arr coords)
  (setq value (vl-catch-all-apply 'vla-AddLightWeightPolyline (list (opor-ms) arr)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq pl value)
      (setq closed-result
        (vl-catch-all-apply 'vla-put-Closed (list pl :vlax-true)))
      (setq layer-result
        (vl-catch-all-apply 'vla-put-Layer (list pl *opor-layer-level-lines*)))
      (if (or (vl-catch-all-error-p closed-result)
              (vl-catch-all-error-p layer-result))
        (progn
          (opor-delete-object pl)
          nil)
        (progn
          (setq registered (opor-register-created pl "tin-triangle"))
          (if (and registered
                   (= (opor-object-xdata-type pl) "tin-triangle"))
            (opor-tin-track-pending-object pl)
            (progn
              (opor-unregister-created pl)
              (opor-delete-object pl)
              nil)))))))

(defun opor-tin-insert-triangles (triangles / objects failures tri pl)
  (setq objects '() failures 0)
  (foreach tri triangles
    (setq pl (opor-tin-create-polyline tri))
    (if pl
      (setq objects (cons pl objects))
      (setq failures (1+ failures))))
  (list (reverse objects) failures))

;; --- Автоматические блоки уклона по плоскости TIN ---

(defun opor-tin-normalize-angle (angle)
  (while (< angle 0.0) (setq angle (+ angle (* 2.0 pi))))
  (while (>= angle (* 2.0 pi)) (setq angle (- angle (* 2.0 pi))))
  angle)

(defun opor-tin-triangle-slope (tri marks / a b c az bz cz dx1 dy1 dx2 dy2 dz1 dz2 det gx gy percent angle)
  (setq a (car tri) b (cadr tri) c (caddr tri))
  (setq az (opor-level-mark-height marks a))
  (setq bz (opor-level-mark-height marks b))
  (setq cz (opor-level-mark-height marks c))
  (setq dx1 (- (car b) (car a)))
  (setq dy1 (- (cadr b) (cadr a)))
  (setq dx2 (- (car c) (car a)))
  (setq dy2 (- (cadr c) (cadr a)))
  (setq det (- (* dx1 dy2) (* dx2 dy1)))
  (if (or (not (numberp az)) (not (numberp bz)) (not (numberp cz))
          (equal det 0.0 1e-12))
    nil
    (progn
      (setq dz1 (- bz az) dz2 (- cz az))
      ;; z = gx*x + gy*y + c; вектор (gx,gy) направлен снизу вверх.
      (setq gx (/ (- (* dz1 dy2) (* dz2 dy1)) det))
      (setq gy (/ (- (* dx1 dz2) (* dx2 dz1)) det))
      (setq percent (* 100.0 (sqrt (+ (* gx gx) (* gy gy)))))
      (setq angle
        (if (equal percent 0.0 1e-12)
          0.0
          (opor-tin-normalize-angle (atan gy gx))))
      (list
        (cons 'point (opor-tin-centroid tri))
        (cons 'percent percent)
        (cons 'gx gx)
        (cons 'gy gy)
        (cons 'angle angle)))))

(defun opor-tin-slope-records (triangles marks / result failures tri record)
  (setq result '() failures 0)
  (foreach tri triangles
    (setq record (opor-tin-triangle-slope tri marks))
    (if record
      (setq result (cons record result))
      (setq failures (1+ failures))))
  (list (reverse result) failures))

(defun opor-tin-format-percent (percent)
  (strcat (itoa (opor-round percent)) "%"))

(defun opor-tin-set-slope-text (block text / raw atts value)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (vl-catch-all-error-p raw)
    nil
    (progn
      (setq atts (opor-variant-list raw))
      (if (not atts)
        nil
        (progn
          (setq value
            (vl-catch-all-apply 'vla-put-TextString (list (car atts) text)))
          (not (vl-catch-all-error-p value)))))))

(defun opor-tin-slope-rotation (desired-axis base-axis percent)
  (if (equal percent 0.0 1e-12)
    0.0
    (opor-tin-normalize-angle (- desired-axis base-axis))))

;; Точные числа дописываются после трёх стандартных полей OPOR-XData.
;; OPORCLEAN/OPORDUMP читают первые поля и остаются совместимыми.
(defun opor-tin-mark-slope-data (block record / en old entry app-data new-entry value)
  (setq en (opor-object-ename block))
  (if (or (not en) (vl-catch-all-error-p en))
    nil
    (progn
      (setq old (entget en (list *opor-xdata-app*)))
      (setq entry (assoc -3 old))
      (setq app-data (if entry (cadr entry) nil))
      (if (and app-data (= (car app-data) *opor-xdata-app*))
        (progn
          (setq new-entry
            (list -3
              (append app-data
                (list
                  (cons 1000 "TIN_SLOPE_DEFINED")
                  (cons 1040 (cdr (assoc 'percent record)))
                  (cons 1040 (cdr (assoc 'gx record)))
                  (cons 1040 (cdr (assoc 'gy record)))))))
          (setq value
            (vl-catch-all-apply 'entmod (list (subst new-entry entry old)))))
        (setq value nil))
      (and value (not (vl-catch-all-error-p value))))))

(defun opor-tin-insert-slope (record / pt percent angle base-axis rotation value block ok)
  (setq pt (cdr (assoc 'point record)))
  (setq percent (cdr (assoc 'percent record)))
  (setq angle (cdr (assoc 'angle record)))
  ;; У разных версий блока свойство Угол может быть read-only. Вставляем блок
  ;; без поворота, читаем его реальную штатную ось и доворачиваем весь reference.
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list (opor-ms) (vlax-3d-point pt) "slope" 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    (progn
      (opor-log
        (strcat "TIN slope: InsertBlock завершился ошибкой: "
          (vl-catch-all-error-message value)))
      nil)
    (progn
      (setq block value)
      (vl-catch-all-apply 'vla-put-Layer (list block *opor-layer-level-lines*))
      (vl-catch-all-apply 'vla-put-Color (list block 256))
      (setq base-axis (opor-write-level-axis-angle block))
      (setq rotation (opor-tin-slope-rotation angle base-axis percent))
      (setq value
        (vl-catch-all-apply 'vla-put-Rotation (list block rotation)))
      (setq ok (not (vl-catch-all-error-p value)))
      (if (not ok)
        (opor-log
          (strcat "TIN slope: не удалось повернуть block reference: "
            (vl-catch-all-error-message value))))
      (if ok
        (progn
          (setq ok
            (opor-tin-set-slope-text block (opor-tin-format-percent percent)))
          (if (not ok)
            (opor-log "TIN slope: не удалось записать атрибут процента."))))
      (if ok
        (progn
          (setq block (opor-register-created block "tin-slope"))
          (if (and block (= (opor-object-xdata-type block) "tin-slope")
                   (opor-tin-mark-slope-data block record))
            (opor-tin-track-pending-object block)
            (progn
              (opor-log "TIN slope: не удалось записать точный градиент в OPOR-XData.")
              (opor-unregister-created block)
              (opor-delete-object block)
              nil)))
        (progn
          (opor-delete-object block)
          nil)))))

(defun opor-tin-insert-slopes (records / blocks failures record block)
  (setq blocks '() failures 0)
  (foreach record records
    (setq block (opor-tin-insert-slope record))
    (if block
      (setq blocks (cons block blocks))
      (setq failures (1+ failures))))
  (list (reverse blocks) failures))

(defun opor-tin-existing-slopes (boundary holes / objects obj pt)
  (setq objects '())
  (vlax-for obj (opor-ms)
    (if (= (opor-object-xdata-type obj) "tin-slope")
      (progn
        (setq pt (opor-slope-insertion-point obj))
        (if (and pt (opor-point-in-working-area-p pt boundary holes))
          (setq objects (cons obj objects))))))
  (reverse objects))

(defun opor-tin-delete-slope-blocks (objects / count obj)
  (opor-tin-delete-blocks objects))

(defun opor-tin-object-centroid (obj / pts)
  (setq pts (opor-polyline-vertices obj))
  (if (= (length pts) 3) (opor-tin-centroid pts) nil))

(defun opor-tin-existing-triangles (boundary holes / objects obj pt)
  (setq objects '() count 0)
  (vlax-for obj (opor-ms)
    (if (= (opor-object-xdata-type obj) "tin-triangle")
      (progn
        (setq pt (opor-tin-object-centroid obj))
        (if (and pt (opor-point-in-working-area-p pt boundary holes))
          (setq objects (cons obj objects))))))
  (reverse objects))

(defun opor-tin-delete-existing (boundary holes)
  (opor-tin-delete-blocks (opor-tin-existing-triangles boundary holes)))

(defun opor-pick-boundary-with-levels-hidden (/ state value)
  (setq state (opor-slope-layer-state *opor-layer-level-lines*))
  (opor-slope-layer-set-on *opor-layer-level-lines* nil)
  (setq value (vl-catch-all-apply 'opor-select-outer-boundary nil))
  (opor-slope-layer-put-state state)
  (if (vl-catch-all-error-p value) nil value))

(defun opor-tin-run (/ boundary holes raw-marks source-raw-marks old-generated-marks prepared marks conflicts
                       contour-points missing-contour hole-points missing-holes curve-target-points source-points outer-curve-marks preliminary-marks preliminary
                       interpolation surface-generated-marks boundary-interpolation boundary-generated-marks generated-marks unresolved resolution-mode
                       manual-input manual-marks new-curve-marks curve-marks constraint-retry-marks constraint-retry-count
                       points raw-triangles constraints constrained
                       flips failures triangles rejected slope-data slope-records slope-calc-failures
                       persisted-marks insertion inserted-blocks insert-failures old-slope-blocks old-triangle-blocks
                       slope-insertion inserted-slopes slope-insert-failures triangle-insertion inserted-triangles triangle-insert-failures
                       replaced-marks replaced-slopes removed created cleanup-warnings)
  (opor-tin-clear-pending-objects)
  (opor-view-save)
  (if (not (and (opor-import-level-block) (opor-import-slope-block)))
    (progn
      (opor-alert
        "Не удалось загрузить блоки отметки/уклона из библиотеки OPOR. TIN не построен.")
      nil)
    (progn
      (setq boundary (opor-pick-boundary-with-levels-hidden))
      (if (not boundary)
        nil
        (progn
      (opor-session-set 'outer-boundary boundary)
      (opor-zoom-to-boundary boundary)
      (setq holes (opor-detect-holes boundary))
      (if (not (opor-hole-regions-valid-p holes))
        (progn
          (opor-log "TIN остановлен: некорректная геометрия проёмов.")
          nil)
        (progn
          (opor-session-set 'holes holes)
          (setq raw-marks (opor-level-read-marks boundary))
          (setq old-generated-marks
            (opor-tin-old-generated-marks raw-marks boundary))
          (setq source-raw-marks (opor-tin-source-marks raw-marks))
          (setq prepared (opor-tin-prepare-marks source-raw-marks boundary holes))
          (setq marks (car prepared) conflicts (cadr prepared))
          (setq contour-points
            (opor-unique-points
              (opor-polyline-vertices boundary)
              *opor-vba-point-dedupe-tolerance*))
          (setq missing-contour
            (opor-level-unmarked-points contour-points marks))
          (setq hole-points '())
          (foreach hole holes
            (setq hole-points
              (append hole-points (opor-polyline-vertices hole))))
          (setq hole-points
            (opor-unique-points hole-points *opor-vba-point-dedupe-tolerance*))
          (setq missing-holes (opor-level-unmarked-points hole-points marks))
          (cond
            (conflicts
              (opor-error-circles conflicts)
              (opor-alert
                (strcat
                  "В одной точке найдены отметки с разными значениями.\n"
                  "Конфликтов: " (itoa (length conflicts)) "."))
              nil)
            (missing-contour
              (opor-error-circles missing-contour)
              (opor-alert
                (strcat
                  "Для TIN нужны отметки во всех вершинах внешнего контура.\n"
                  "Не хватает отметок: " (itoa (length missing-contour)) "."))
              nil)
            ((< (length marks) 3)
              (opor-alert "Для построения TIN нужно минимум три уникальные отметки.")
              nil)
            (t
              (setq curve-target-points
                (opor-polyline-linearized-vertices
                  boundary *opor-curve-chord-tolerance*))
              (foreach hole holes
                (setq curve-target-points
                  (append curve-target-points
                    (opor-polyline-linearized-vertices
                      hole *opor-curve-chord-tolerance*))))
              (setq curve-target-points
                (opor-unique-points
                  curve-target-points *opor-vba-point-dedupe-tolerance*))
              (setq marks
                (opor-tin-snap-marks-to-points
                  marks curve-target-points
                  *opor-vba-mark-match-tolerance*))
              (setq marks
                (opor-tin-project-near-marks-to-boundary
                  marks boundary *opor-point-tolerance*))
              (setq source-points
                (mapcar '(lambda (mark) (cdr (assoc 'point mark))) marks))
              ;; Выпуклая дуга может выходить за convex hull своих концов. Её
              ;; расчётные точки нужны уже предварительному TIN, иначе вершина
              ;; проёма в округлой части ошибочно окажется без поверхности.
              (setq outer-curve-marks
                (opor-tin-curve-sample-marks (list boundary) marks))
              (setq preliminary-marks (append marks outer-curve-marks))
              (setq preliminary
                (opor-tin-delaunay
                  (mapcar
                    '(lambda (mark) (cdr (assoc 'point mark)))
                    preliminary-marks)))
              (setq interpolation
                (opor-tin-interpolate-hole-marks
                  missing-holes preliminary preliminary-marks))
              (setq surface-generated-marks (car interpolation))
              (setq boundary-generated-marks '())
              (setq generated-marks surface-generated-marks)
              (setq unresolved (cadr interpolation))
              (setq manual-marks '() manual-input nil)
              (if unresolved
                (progn
                  (setq resolution-mode
                    (opor-tin-prompt-hole-mark-mode unresolved))
                  (cond
                    ((= resolution-mode "Авто")
                      (setq boundary-interpolation
                        (opor-tin-interpolate-boundary-hole-marks
                          unresolved boundary marks))
                      (setq boundary-generated-marks
                        (car boundary-interpolation))
                      (setq unresolved (cadr boundary-interpolation))
                      (setq generated-marks
                        (append generated-marks boundary-generated-marks))
                      (opor-log
                        (strcat
                          "TIN: автоматически рассчитано по отметкам внешнего контура="
                          (itoa (length boundary-generated-marks))
                          ", осталось без отметки=" (itoa (length unresolved)) "."))
                      (if unresolved
                        (progn
                          (opor-alert
                            (strcat
                              "По внешнему контуру автоматически рассчитано: "
                              (itoa (length boundary-generated-marks)) ".\n"
                              "Осталось ввести вручную: "
                              (itoa (length unresolved)) "."))
                          (setq manual-input
                            (opor-tin-prompt-manual-hole-marks
                              unresolved boundary (car prepared)))
                          (setq manual-marks (car manual-input)))))
                    ((= resolution-mode "Вручную")
                      (setq manual-input
                        (opor-tin-prompt-manual-hole-marks
                          unresolved boundary (car prepared)))
                      (setq manual-marks (car manual-input)))
                    (t
                      (setq manual-input (list '() T))))))
              (if (and unresolved (cadr manual-input))
                nil
                (progn
                  (setq marks
                    (append marks generated-marks manual-marks outer-curve-marks))
                  (setq new-curve-marks
                    (opor-tin-curve-sample-marks
                      (cons boundary holes) marks))
                  (setq curve-marks
                    (append outer-curve-marks new-curve-marks))
                  (setq constraint-retry-marks '() constraint-retry-count 0)
                  (setq marks (append marks new-curve-marks))
                  (setq points
                    (mapcar '(lambda (mark) (cdr (assoc 'point mark))) marks))
                  (setq raw-triangles (opor-tin-delaunay points))
                  (setq constraints (opor-tin-build-constraints boundary holes points))
                  (setq constrained (opor-tin-apply-constraints raw-triangles constraints))
                  (setq flips (cadr constrained) failures (caddr constrained))
                  (setq triangles
                    (opor-tin-filter-triangles (car constrained) boundary holes))
                  (setq rejected (- (length raw-triangles) (length triangles)))
                  (if failures
                    (progn
                      (setq constraint-retry-marks
                        (opor-tin-failed-boundary-constraint-marks
                          failures boundary marks))
                      (setq constraint-retry-count
                        (length constraint-retry-marks))
                      (if (> constraint-retry-count 0)
                        (progn
                          (opor-log
                            (strcat
                              "TIN retry: добавлено технических точек длинных внешних рёбер="
                              (itoa constraint-retry-count) "."))
                          (setq marks (append marks constraint-retry-marks))
                          (setq curve-marks
                            (append curve-marks constraint-retry-marks))
                          (setq points
                            (mapcar
                              '(lambda (mark) (cdr (assoc 'point mark)))
                              marks))
                          (setq raw-triangles (opor-tin-delaunay points))
                          (setq constraints
                            (opor-tin-build-constraints boundary holes points))
                          (setq constrained
                            (opor-tin-apply-constraints raw-triangles constraints))
                          (setq flips (cadr constrained)
                                failures (caddr constrained))
                          (setq triangles
                            (opor-tin-filter-triangles
                              (car constrained) boundary holes))
                          (setq rejected
                            (- (length raw-triangles) (length triangles)))
                          (opor-log
                            (strcat
                              "TIN retry завершён: проблемных рёбер="
                              (itoa (length failures)) "."))))))
                  (if failures
                    (progn
                      (opor-tin-rollback-new-objects '() '() '() manual-marks)
                      (opor-tin-show-failed-constraints failures)
                      (opor-alert
                        (strcat
                          "TIN не построен: не удалось восстановить рёбра контура.\n"
                          "Проблемных рёбер: " (itoa (length failures)) ".\n"
                          "Концы отмечены оранжевыми кругами."))
                      nil)
                    (if (not triangles)
                      (progn
                        (opor-tin-rollback-new-objects '() '() '() manual-marks)
                        (opor-alert "TIN не построен: точки вырождены или треугольники вне рабочей области.")
                        nil)
                      (progn
                        (opor-ensure-layer *opor-layer-level-lines* 8 "Continuous")
                        (setq slope-data (opor-tin-slope-records triangles marks))
                        (setq slope-records (car slope-data))
                        (setq slope-calc-failures (cadr slope-data))
                        (if (> slope-calc-failures 0)
                          (progn
                            (opor-tin-rollback-new-objects '() '() '() manual-marks)
                            (opor-alert
                              (strcat
                                "TIN не построен: не удалось вычислить уклон треугольников.\n"
                                "Ошибок расчёта: " (itoa slope-calc-failures) "."))
                            nil)
                          (progn
                            ;; Старый TIN трогаем только после полного создания
                            ;; и проверки всех новых отметок, уклонов и треугольников.
                            (setq old-slope-blocks
                              (opor-tin-existing-slopes boundary holes))
                            (setq old-triangle-blocks
                              (opor-tin-existing-triangles boundary holes))
                            (setq persisted-marks
                              (append generated-marks curve-marks))
                            (setq insertion
                              (opor-tin-insert-generated-marks persisted-marks (car prepared)))
                            (setq inserted-blocks (car insertion))
                            (setq insert-failures (cadr insertion))
                            (if (> insert-failures 0)
                              (progn
                                (opor-tin-rollback-new-objects
                                  '() '() inserted-blocks manual-marks)
                                (opor-alert
                                  (strcat
                                    "TIN не построен: не удалось создать рассчитанные отметки проёмов/дуг.\n"
                                    "Ошибок вставки: " (itoa insert-failures) "."))
                                nil)
                              (progn
                                (setq slope-insertion (opor-tin-insert-slopes slope-records))
                                (setq inserted-slopes (car slope-insertion))
                                (setq slope-insert-failures (cadr slope-insertion))
                                (if (> slope-insert-failures 0)
                                  (progn
                                    (opor-tin-rollback-new-objects
                                      '() inserted-slopes inserted-blocks manual-marks)
                                    (opor-alert
                                      (strcat
                                        "TIN не построен: не удалось создать блоки уклонов.\n"
                                        "Ошибок вставки: " (itoa slope-insert-failures) "."))
                                    nil)
                                  (progn
                                    (setq triangle-insertion
                                      (opor-tin-insert-triangles triangles))
                                    (setq inserted-triangles (car triangle-insertion))
                                    (setq triangle-insert-failures (cadr triangle-insertion))
                                    (if (> triangle-insert-failures 0)
                                      (progn
                                        (opor-tin-rollback-new-objects
                                          inserted-triangles inserted-slopes inserted-blocks manual-marks)
                                        (opor-alert
                                          (strcat
                                            "TIN не построен: не удалось создать все треугольники.\n"
                                            "Ошибок вставки: " (itoa triangle-insert-failures) "."))
                                        nil)
                                      (progn
                                    (setq created (length inserted-triangles))
                                    (setq replaced-marks
                                      (opor-tin-delete-old-generated-marks old-generated-marks))
                                    (setq replaced-slopes
                                      (opor-tin-delete-slope-blocks old-slope-blocks))
                                    (setq removed (opor-tin-delete-blocks old-triangle-blocks))
                                    (setq cleanup-warnings
                                      (+ (- (length old-generated-marks) replaced-marks)
                                         (- (length old-slope-blocks) replaced-slopes)
                                         (- (length old-triangle-blocks) removed)))
                                    (opor-session-set 'tin-input-mark-count (length source-raw-marks))
                                    (opor-session-set 'tin-source-mark-count
                                      (+ (length source-points) (length manual-marks)))
                                    (opor-session-set 'tin-interpolated-mark-count (length generated-marks))
                                    (opor-session-set 'tin-boundary-interpolated-mark-count
                                      (length boundary-generated-marks))
                                    (opor-session-set 'tin-manual-hole-mark-count (length manual-marks))
                                    (opor-session-set 'tin-curve-sample-count (length curve-marks))
                                    (opor-session-set 'tin-constraint-retry-mark-count
                                      constraint-retry-count)
                                    (opor-session-set 'tin-replaced-mark-count replaced-marks)
                                    (opor-session-set 'tin-point-count (length points))
                                    (opor-session-set 'tin-raw-triangle-count (length raw-triangles))
                                    (opor-session-set 'tin-constraint-count (length constraints))
                                    (opor-session-set 'tin-constraint-flip-count flips)
                                    (opor-session-set 'tin-rejected-triangle-count rejected)
                                    (opor-session-set 'tin-removed-triangle-count removed)
                                    (opor-session-set 'tin-triangle-count created)
                                    (opor-session-set 'tin-slope-count (length inserted-slopes))
                                    (opor-session-set 'tin-replaced-slope-count replaced-slopes)
                                    (opor-session-set 'tin-replacement-cleanup-errors cleanup-warnings)
                                    (opor-tin-clear-pending-objects)
                                    (opor-log
                                      (strcat
                                        "TIN завершён: исходных отметок="
                                        (itoa (+ (length source-points) (length manual-marks)))
                                        ", интерполировано для проёмов=" (itoa (length generated-marks))
                                        ", из них по внешней границе="
                                        (itoa (length boundary-generated-marks))
                                        ", введено вручную=" (itoa (length manual-marks))
                                        ", точек дуг="
                                        (itoa (- (length curve-marks) constraint-retry-count))
                                        ", точек retry рёбер=" (itoa constraint-retry-count)
                                        ", отметок всего=" (itoa (length points))
                                        ", треугольников=" (itoa created)
                                        ", уклонов=" (itoa (length inserted-slopes))
                                        ", вне области=" (itoa rejected)
                                        ", рёбер=" (itoa (length constraints))
                                        ", flips=" (itoa flips)
                                        ", заменено треугольников=" (itoa removed)
                                        ", заменено уклонов=" (itoa replaced-slopes)
                                        ", заменено автоотметок=" (itoa replaced-marks)
                                        ", ошибок очистки старого TIN=" (itoa cleanup-warnings) "."))
                                    (opor-alert
                                      (strcat
                                        "TIN построен.\nИсходных отметок: "
                                        (itoa (+ (length source-points) (length manual-marks)))
                                        "\nИнтерполировано для проёмов: " (itoa (length generated-marks))
                                        "\nИз них по внешней границе: "
                                        (itoa (length boundary-generated-marks))
                                        "\nВведено вручную: " (itoa (length manual-marks))
                                        "\nРасчётных точек дуг: "
                                        (itoa (- (length curve-marks) constraint-retry-count))
                                        "\nТехнических точек длинных рёбер: "
                                        (itoa constraint-retry-count)
                                        "\nТреугольников: " (itoa created)
                                        "\nБлоков уклона: " (itoa (length inserted-slopes))
                                        (if (> cleanup-warnings 0)
                                          (strcat "\nНе удалось удалить старых объектов: "
                                            (itoa cleanup-warnings) ".")
                                          "")))
                                    (= cleanup-warnings 0)))))))))))))))))))))))

(defun opor-command-tin ()
  (opor-init-session)
  (opor-tin-run))

(princ)
