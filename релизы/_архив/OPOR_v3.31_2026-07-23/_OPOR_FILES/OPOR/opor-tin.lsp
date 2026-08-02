;;; OPOR TIN: препроцессор областей высот по блокам отметок (Делоне V1).

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

(defun opor-tin-build-constraints (boundary holes points / result hole)
  (setq result
    (opor-tin-ring-constraints (opor-polyline-vertices boundary) points))
  (foreach hole holes
    (setq result
      (append result
        (opor-tin-ring-constraints (opor-polyline-vertices hole) points))))
  result)

(defun opor-tin-apply-constraints (triangles constraints / flips failures edge recovered)
  (setq flips 0 failures '())
  (foreach edge constraints
    (setq recovered (opor-tin-recover-edge edge triangles))
    (setq triangles (car recovered))
    (setq flips (+ flips (cadr recovered)))
    (if (not (caddr recovered))
      (setq failures (cons edge failures))))
  (list triangles flips (reverse failures)))

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

;; Отметки, рассчитанные предыдущим запуском TIN, не участвуют как исходные:
;; при повторном запуске их высота должна быть пересчитана по пользовательским
;; отметкам, а не сохранена как устаревшее значение.
(defun opor-tin-generated-mark-p (mark / obj)
  (setq obj (cdr (assoc 'object mark)))
  (and obj
       (= (opor-object-xdata-type obj) "tin-interpolated-mark")))

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

(defun opor-tin-insert-generated-mark (mark template / pt sx sy sz rotation layer color value block)
  (setq pt (cdr (assoc 'point mark)))
  (setq sx (if template (vla-get-XScaleFactor template) 1.0))
  (setq sy (if template (vla-get-YScaleFactor template) 1.0))
  (setq sz (if template (vla-get-ZScaleFactor template) 1.0))
  (setq rotation (if template (vla-get-Rotation template) 0.0))
  (setq layer (if template (vla-get-Layer template) "0"))
  (setq color (if template (vla-get-Color template) 256))
  (if (= color 1) (setq color 256))
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
          (opor-register-created block "tin-interpolated-mark")
          block)
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

(defun opor-tin-delete-blocks (blocks / count block)
  (setq count 0)
  (foreach block blocks
    (if (opor-object-live-p block)
      (progn
        (opor-unregister-created block)
        (opor-delete-object block)
        (setq count (1+ count)))))
  count)

(defun opor-tin-delete-old-generated-marks (marks / blocks mark obj)
  (setq blocks '())
  (foreach mark marks
    (setq obj (cdr (assoc 'object mark)))
    (if obj (setq blocks (cons obj blocks))))
  (opor-tin-delete-blocks blocks))

(defun opor-tin-create-polyline (tri / coords arr value pl)
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
      (vl-catch-all-apply 'vla-put-Closed (list pl :vlax-true))
      (vl-catch-all-apply 'vla-put-Layer (list pl *opor-layer-level-lines*))
      (opor-register-created pl "tin-triangle")
      pl)))

(defun opor-tin-object-centroid (obj / pts)
  (setq pts (opor-polyline-vertices obj))
  (if (= (length pts) 3) (opor-tin-centroid pts) nil))

(defun opor-tin-delete-existing (boundary holes / objects obj pt count)
  (setq objects '() count 0)
  (vlax-for obj (opor-ms)
    (if (= (opor-object-xdata-type obj) "tin-triangle")
      (progn
        (setq pt (opor-tin-object-centroid obj))
        (if (and pt (opor-point-in-working-area-p pt boundary holes))
          (setq objects (cons obj objects))))))
  (foreach obj objects
    (opor-delete-object obj)
    (setq count (1+ count)))
  count)

(defun opor-pick-boundary-with-levels-hidden (/ state value)
  (setq state (opor-slope-layer-state *opor-layer-level-lines*))
  (opor-slope-layer-set-on *opor-layer-level-lines* nil)
  (setq value (vl-catch-all-apply 'opor-select-outer-boundary nil))
  (opor-slope-layer-put-state state)
  (if (vl-catch-all-error-p value) nil value))

(defun opor-tin-run (/ boundary holes raw-marks source-raw-marks old-generated-marks prepared marks conflicts
                       contour-points missing-contour hole-points missing-holes source-points preliminary
                       interpolation generated-marks unresolved points raw-triangles constraints constrained
                       flips failures triangles rejected insertion inserted-blocks insert-failures replaced-marks
                       removed created tri pl)
  (opor-view-save)
  (setq boundary (opor-pick-boundary-with-levels-hidden))
  (if (not boundary)
    nil
    (progn
      (opor-session-set 'outer-boundary boundary)
      (opor-zoom-to-boundary boundary)
      (setq holes (opor-detect-holes boundary))
      (if (not (opor-hole-regions-valid-p holes))
        nil
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
              (setq source-points
                (mapcar '(lambda (mark) (cdr (assoc 'point mark))) marks))
              (setq preliminary (opor-tin-delaunay source-points))
              (setq interpolation
                (opor-tin-interpolate-hole-marks
                  missing-holes preliminary marks))
              (setq generated-marks (car interpolation))
              (setq unresolved (cadr interpolation))
              (if unresolved
                (progn
                  (opor-error-circles unresolved)
                  (opor-alert
                    (strcat
                      "TIN не построен: не удалось вычислить отметки вершин проёмов.\n"
                      "Проблемных вершин: " (itoa (length unresolved)) "."))
                  nil)
                (progn
                  (setq marks (append marks generated-marks))
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
                      (opor-alert
                        (strcat
                          "TIN не построен: не удалось восстановить рёбра контура.\n"
                          "Проблемных рёбер: " (itoa (length failures)) "."))
                      nil)
                    (if (not triangles)
                      (progn
                        (opor-alert "TIN не построен: точки вырождены или треугольники вне рабочей области.")
                        nil)
                      (progn
                        (opor-ensure-layer *opor-layer-level-lines* 8 "Continuous")
                        (setq insertion
                          (opor-tin-insert-generated-marks generated-marks (car prepared)))
                        (setq inserted-blocks (car insertion))
                        (setq insert-failures (cadr insertion))
                        (if (> insert-failures 0)
                          (progn
                            (opor-tin-delete-blocks inserted-blocks)
                            (opor-alert
                              (strcat
                                "TIN не построен: не удалось создать рассчитанные отметки проёмов.\n"
                                "Ошибок вставки: " (itoa insert-failures) "."))
                            nil)
                          (progn
                            (setq replaced-marks
                              (opor-tin-delete-old-generated-marks old-generated-marks))
                            (setq removed (opor-tin-delete-existing boundary holes))
                            (setq created 0)
                            (foreach tri triangles
                              (setq pl (opor-tin-create-polyline tri))
                              (if pl (setq created (1+ created))))
                            (opor-session-set 'tin-input-mark-count (length source-raw-marks))
                            (opor-session-set 'tin-source-mark-count (length source-points))
                            (opor-session-set 'tin-interpolated-mark-count (length generated-marks))
                            (opor-session-set 'tin-replaced-mark-count replaced-marks)
                            (opor-session-set 'tin-point-count (length points))
                            (opor-session-set 'tin-raw-triangle-count (length raw-triangles))
                            (opor-session-set 'tin-constraint-count (length constraints))
                            (opor-session-set 'tin-constraint-flip-count flips)
                            (opor-session-set 'tin-rejected-triangle-count rejected)
                            (opor-session-set 'tin-removed-triangle-count removed)
                            (opor-session-set 'tin-triangle-count created)
                            (opor-log
                              (strcat
                                "TIN завершён: исходных отметок=" (itoa (length source-points))
                                ", рассчитано для проёмов=" (itoa (length generated-marks))
                                ", отметок всего=" (itoa (length points))
                                ", треугольников=" (itoa created)
                                ", вне области=" (itoa rejected)
                                ", рёбер=" (itoa (length constraints))
                                ", flips=" (itoa flips)
                                ", заменено треугольников=" (itoa removed)
                                ", заменено автоотметок=" (itoa replaced-marks) "."))
                            (opor-alert
                              (strcat
                                "TIN построен.\nИсходных отметок: " (itoa (length source-points))
                                "\nРассчитано для проёмов: " (itoa (length generated-marks))
                                "\nТреугольников: " (itoa created)))
                            (= created (length triangles))))))))))))))))

(defun opor-command-tin ()
  (opor-init-session)
  (opor-tin-run))

(princ)
