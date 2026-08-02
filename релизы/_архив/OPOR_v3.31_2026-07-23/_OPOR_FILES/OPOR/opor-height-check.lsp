;;; OPOR h/check_height: диагностическая проверка одной опоры по треугольнику.

(defun opor-height-check-pick-polyline (/ picked obj)
  (setq picked (entsel "\nУкажите внешний контур: "))
  (if picked
    (progn
      (setq obj (opor-to-vla (car picked)))
      (if (opor-polyline-object-p obj)
        obj
        (progn (opor-alert "Это не полилиния.") nil)))
    nil))

(defun opor-height-check-pick-support (/ picked obj)
  (setq picked (entsel "\nУкажите опору внутри области: "))
  (if picked (opor-to-vla (car picked)) nil))

(defun opor-height-check-support-name (obj / name)
  (setq name (strcase (opor-effective-block-name obj)))
  (if (member name '("OPOR_SYMB" "OPOR_SYMB3D")) name nil))

(defun opor-height-check-validate-support (obj / name)
  (cond
    ((not obj) nil)
    ((/= (opor-obj-name obj) "AcDbBlockReference")
      (opor-alert "Это не блок.")
      nil)
    ((not (setq name (opor-height-check-support-name obj)))
      (opor-alert "Это не блок опоры Level/3D.")
      nil)
    (t name)))

(defun opor-height-check-point (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p value)
    nil
    (opor-2d (opor-variant-list value))))

(defun opor-height-check-scale (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-XScaleFactor (list obj)))
  (if (or (vl-catch-all-error-p value) (not (numberp value))) 1.0 value))

(defun opor-height-check-support-value (obj / text value)
  (setq text (opor-first-attribute-text obj))
  (setq value (opor-parse-real text nil))
  (if (numberp value) (opor-round-half-even value) nil))

;; Возвращает область, символ 'boundary или nil.
(defun opor-height-check-containing-area (pt areas / found boundary-p)
  (setq found nil boundary-p nil)
  (foreach area areas
    (if (and (not found) (not boundary-p))
      (cond
        ((opor-point-on-curve-p pt area *opor-slope-boundary-tolerance*)
          (setq boundary-p T))
        ((opor-point-inside-boundary-p pt area)
          (setq found area)))))
  (cond (boundary-p 'boundary) (found found) (t nil)))

(defun opor-height-check-containing-triangle (pt triangles / found)
  (setq found nil)
  (foreach tri triangles
    (if (and (not found) (opor-point-in-triangle-p pt tri))
      (setq found tri)))
  found)

(defun opor-height-check-point-near-p (a b tol)
  (and
    (< (abs (- (car a) (car b))) tol)
    (< (abs (- (cadr a) (cadr b))) tol)))

(defun opor-height-check-find-support-at (name pt / found obj objpt)
  (setq found nil)
  (vlax-for obj (opor-ms)
    (if (and
          (not found)
          (= (opor-obj-name obj) "AcDbBlockReference")
          (= (strcase (opor-effective-block-name obj)) name))
      (progn
        (setq objpt (opor-height-check-point obj))
        (if (and objpt (opor-height-check-point-near-p objpt pt 1.0))
          (setq found obj)))))
  found)

(defun opor-height-check-triangle-points (tri)
  (list
    (cdr (assoc 'a tri))
    (cdr (assoc 'b tri))
    (cdr (assoc 'c tri))))

(defun opor-height-check-find-vertex-supports (name tri / result obj)
  (setq result '())
  (foreach pt (opor-height-check-triangle-points tri)
    (setq obj (opor-height-check-find-support-at name pt))
    (if obj
      (setq result (append result (list obj)))
      (setq result nil)))
  (if (= (length result) 3) result nil))

(defun opor-height-check-shift-point (pt delta)
  (list (+ (car pt) (car delta)) (+ (cadr pt) (cadr delta)) 0.0))

(defun opor-height-check-draw-triangle (tri delta / points coords arr pl layer)
  (setq points (opor-height-check-triangle-points tri))
  (setq coords '())
  (foreach pt points
    (setq pt (opor-height-check-shift-point pt delta))
    (setq coords (append coords (list (car pt) (cadr pt)))))
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill arr coords)
  (setq pl
    (vl-catch-all-apply 'vla-AddLightWeightPolyline (list (opor-ms) arr)))
  (if (vl-catch-all-error-p pl)
    nil
    (progn
      (vla-put-Closed pl :vlax-true)
      (setq layer
        (if (opor-layer-exists-p *opor-layer-triangles*)
          *opor-layer-triangles*
          "0"))
      (vl-catch-all-apply 'vla-put-Layer (list pl layer))
      (opor-register-created pl "height-check-triangle")
      pl)))

(defun opor-height-check-copy-area (area from to / copy result)
  (setq result (vl-catch-all-apply 'vla-Copy (list area)))
  (if (vl-catch-all-error-p result)
    nil
    (progn
      (setq copy result)
      (vl-catch-all-apply
        'vla-Move
        (list copy (vlax-3d-point from) (vlax-3d-point to)))
      (opor-register-created copy "height-check-area")
      copy)))

(defun opor-height-check-value-text (value)
  (if (numberp value) (itoa (opor-round-half-even value)) ""))

(defun opor-height-check-insert-block (pt scale color floor-height support-height / value block lev values)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (opor-ms) (vlax-3d-point pt) "проверкаvb3"
        scale scale 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq block value)
      (if color (vl-catch-all-apply 'vla-put-Color (list block color)))
      (setq lev (- floor-height support-height))
      (setq values
        (list
          (cons "LEV" (opor-height-check-value-text lev))
          (cons "H"
            (strcat
              (opor-height-check-value-text floor-height)
              "-"
              (opor-height-check-value-text support-height)))))
      (opor-set-attribute-values block values)
      (opor-register-created block "height-check-block")
      block)))

;; acad_calc округляет XY перед CAL ILP; воспроизводим это без SendCommand.
(defun opor-height-check-rounded-xy (pt)
  (list
    (float (opor-round-half-even (car pt)))
    (float (opor-round-half-even (cadr pt)))
    0.0))

(defun opor-height-check-calc-z (pt tri / calc-tri)
  (setq calc-tri
    (list
      (cons 'a (opor-height-check-rounded-xy (cdr (assoc 'a tri))))
      (cons 'az (cdr (assoc 'az tri)))
      (cons 'b (opor-height-check-rounded-xy (cdr (assoc 'b tri))))
      (cons 'bz (cdr (assoc 'bz tri)))
      (cons 'c (opor-height-check-rounded-xy (cdr (assoc 'c tri))))
      (cons 'cz (cdr (assoc 'cz tri)))))
  (opor-triangle-z-at-point (opor-height-check-rounded-xy pt) calc-tri))

(defun opor-height-check-build-output
  (area tri selected selected-height vertex-supports output
   / selected-pt scale delta heights floor-height calc-z calc-round check-level points idx block created)
  (setq selected-pt (opor-height-check-point selected))
  (setq scale (opor-height-check-scale selected))
  (setq delta (opor-v- output selected-pt))
  (setq heights '())
  (foreach block vertex-supports
    (setq heights (append heights (list (opor-height-check-support-value block)))))
  (if (vl-some 'null heights)
    (progn (opor-alert "Не удалось прочитать высоту опоры в вершине.") nil)
    (progn
      (setq floor-height (+ (cdr (assoc 'az tri)) (car heights)))
      (opor-height-check-draw-triangle tri delta)
      (opor-height-check-copy-area area selected-pt output)
      (setq created 0)
      (if (opor-height-check-insert-block
            output (* scale 10.0) 1 floor-height selected-height)
        (setq created (1+ created)))
      (setq points (opor-height-check-triangle-points tri))
      (setq idx 0)
      (while (< idx 3)
        (if (opor-height-check-insert-block
              (opor-height-check-shift-point (nth idx points) delta)
              (* scale 7.0) nil floor-height (nth idx heights))
          (setq created (1+ created)))
        (setq idx (1+ idx)))
      (setq calc-z (opor-height-check-calc-z selected-pt tri))
      (setq calc-round (if calc-z (opor-round-half-even calc-z) nil))
      (setq check-level (- floor-height selected-height))
      (opor-session-set 'height-check-floor floor-height)
      (opor-session-set 'height-check-level check-level)
      (opor-session-set 'height-check-calc-level calc-round)
      (opor-session-set 'height-check-created-blocks created)
      (if (and calc-round (> (abs (- calc-round check-level)) 1))
        (opor-alert
          (strcat
            "Расчёт уровня опоры не совпал с плоскостью AutoCAD."
            "\nУровень опоры: " (itoa check-level)
            "\nУровень по плоскости: " (itoa calc-round))))
      (opor-log
        (strcat
          "h завершён: floor=" (itoa floor-height)
          ", уровень=" (itoa check-level)
          ", плоскость=" (if calc-round (itoa calc-round) "?")
          ", блоков проверки=" (itoa created) "."))
      T)))

(defun opor-height-check-run
  (/ boundary selected name selected-pt selected-height output marks areas area triangles tri vertex-supports)
  (cond
    ((not (opor-block-exists-p "проверкаvb3"))
      (opor-alert "Не найден блок проверки проверкаvb3.")
      nil)
    (t
      (opor-view-save)
      (setq boundary (opor-height-check-pick-polyline))
      (if (not boundary)
        nil
        (progn
          (opor-zoom-to-boundary boundary)
          (setq selected (opor-height-check-pick-support))
          (setq name (opor-height-check-validate-support selected))
          (if (not name)
            nil
            (progn
              (setq selected-pt (opor-height-check-point selected))
              (setq selected-height (opor-height-check-support-value selected))
              (setq output (getpoint "\nУкажите точку вывода результата: "))
              (cond
                ((not selected-height)
                  (opor-alert "Не удалось прочитать высоту выбранной опоры.")
                  nil)
                ((not output) nil)
                (t
                  (setq output (opor-2d output))
                  (setq marks (opor-level-read-marks boundary))
                  (setq areas (opor-level-read-polylines boundary))
                  (setq area (opor-height-check-containing-area selected-pt areas))
                  (cond
                    ((eq area 'boundary)
                      (opor-alert "Опора на границе. Такой вариант не проверяется.")
                      nil)
                    ((not area)
                      (opor-alert "Не найдена область высот.")
                      nil)
                    ((not marks)
                      (opor-alert "Не найдены блоки отметок.")
                      nil)
                    (t
                      (setq triangles (opor-level-triangulate-poly area marks))
                      (setq tri (opor-height-check-containing-triangle selected-pt triangles))
                      (if (not tri)
                        (progn
                          (opor-alert "Не найден треугольник для выбранной опоры.")
                          nil)
                        (progn
                          (setq vertex-supports
                            (opor-height-check-find-vertex-supports name tri))
                          (if (not vertex-supports)
                            (progn
                              (opor-alert "Не найдены блоки опор в вершинах треугольника.")
                              nil)
                            (opor-height-check-build-output
                              area tri selected selected-height
                              vertex-supports output)))))))))))))))

(defun opor-command-height-check ()
  (opor-init-session)
  (opor-height-check-run))

(princ)
