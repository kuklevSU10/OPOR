;;; OPOR h/write_levl: пересчёт выбранных отметок от воронки и блока уклона.

(defun opor-write-level-block-p (obj name)
  (and obj
       (= (opor-obj-name obj) "AcDbBlockReference")
       (= (strcase (opor-effective-block-name obj)) (strcase name))))

(defun opor-write-level-pick-block (prompt name invalid-message / picked obj done result)
  (setq done nil result nil)
  (while (not done)
    (setq picked (entsel prompt))
    (if (not picked)
      (setq done T)
      (progn
        (setq obj (vlax-ename->vla-object (car picked)))
        (if (opor-write-level-block-p obj name)
          (setq result obj done T)
          (opor-alert invalid-message)))))
  result)

(defun opor-write-level-property-number (prop / value)
  (setq value (vl-catch-all-apply 'vla-get-Value (list prop)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (if (= (type value) 'variant)
        (setq value (vlax-variant-value value)))
      (if (numberp value) value nil))))

(defun opor-write-level-dynamic-angle (block / raw props prop name value angle)
  ;; On Error Resume Next в VBA оставляет angl=0, если свойства нет.
  (setq angle 0.0)
  (setq raw (vl-catch-all-apply 'vla-GetDynamicBlockProperties (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq props (opor-variant-list raw))
      (foreach prop props
        (setq name (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
        (if (and (not (vl-catch-all-error-p name)) (= name "Угол"))
          (progn
            (setq value (opor-write-level-property-number prop))
            (if (numberp value) (setq angle value)))))))
  angle)

(defun opor-write-level-rotation (block / value)
  (setq value (vl-catch-all-apply 'vla-get-Rotation (list block)))
  (if (or (vl-catch-all-error-p value) (not (numberp value))) 0.0 value))

(defun opor-write-level-axis-angle (slope / angle rotation)
  (setq angle (opor-write-level-dynamic-angle slope))
  (setq rotation (opor-write-level-rotation slope))
  ;; k_write_levl: 0→90, 90→180, 270→0, затем Rotation.
  (if (< angle (* 1.5 pi))
    (setq angle (+ angle (/ pi 2.0)))
    (setq angle (- angle (* 1.5 pi))))
  (setq angle (+ angle rotation))
  (if (>= angle (* 2.0 pi)) (setq angle (- angle (* 2.0 pi))))
  angle)

(defun opor-write-level-vba-distance
  (reference target angle
   / xline xline-perp p1 p2 raw ints dx dy inner result)
  ;; k_write_levl считает через реальные XLine/Rotate/IntersectWith. Прямая
  ;; проекция математически та же, но на полуцелых значениях теряет численную
  ;; погрешность AutoCAD и даёт другое VBA Round.
  (setq result nil xline nil xline-perp nil)
  (setq p1 (list (+ (car reference) 100.0) (cadr reference) 0.0))
  (setq p2 (list (+ (car target) 100.0) (cadr target) 0.0))
  (setq raw
    (vl-catch-all-apply
      'vla-AddXline
      (list (opor-ms) (vlax-3d-point reference) (vlax-3d-point p1))))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq xline raw)
      (setq raw
        (vl-catch-all-apply
          'vla-Rotate
          (list xline (vlax-3d-point reference) angle)))
      (if (not (vl-catch-all-error-p raw))
        (progn
          (setq raw
            (vl-catch-all-apply
              'vla-AddXline
              (list (opor-ms) (vlax-3d-point target) (vlax-3d-point p2))))
          (if (not (vl-catch-all-error-p raw))
            (progn
              (setq xline-perp raw)
              (setq raw
                (vl-catch-all-apply
                  'vla-Rotate
                  (list xline-perp (vlax-3d-point target) (+ angle (/ pi 2.0)))))
              (if (not (vl-catch-all-error-p raw))
                (progn
                  (setq raw
                    (vl-catch-all-apply
                      'vla-IntersectWith
                      (list xline-perp xline acExtendNone)))
                  (if (not (vl-catch-all-error-p raw))
                    (progn
                      (setq ints (opor-variant-list raw))
                      (if (>= (length ints) 2)
                        (progn
                          (setq dx (- (car reference) (nth 0 ints)))
                          (setq dy (- (cadr reference) (nth 1 ints)))
                          ;; zz_functions.distance: Sqr((Sqr(x^2+y^2))^2)
                          (setq inner (sqrt (+ (* dx dx) (* dy dy))))
                          (setq result (sqrt (* inner inner)))))))))))))))
  ;; Временные объекты удаляются и на частично неуспешном COM-пути.
  (if xline-perp (vl-catch-all-apply 'vla-Delete (list xline-perp)))
  (if xline (vl-catch-all-apply 'vla-Delete (list xline)))
  result)

(defun opor-write-level-percent (slope / text)
  (setq text (opor-first-attribute-text slope))
  (if text
    (opor-parse-real (opor-string-replace-all "%" "" text) nil)
    nil))

(defun opor-write-level-vba-round (value)
  ;; Один и тот же IntersectWith даёт из AutoLISP 12.50000000000003 /
  ;; 37.50000000000003, а из VBA на ETS9 фактически округляется как значение
  ;; чуть НИЖЕ половины (+112/+137). Компенсация существенно больше COM-шума
  ;; (~1e-14), но пренебрежимо мала в миллиметровом расчёте.
  (opor-round-half-even (- value 1e-9)))

(defun opor-write-level-prefixed-text (value plus-p)
  (strcat (if plus-p "+" "") (opor-height-text value)))

(defun opor-write-level-update-one
  (reference-point reference-value plus-p slope-percent axis-angle block
   / target-point distance delta new-value text result)
  (setq target-point (opor-slope-insertion-point block))
  (if target-point
    (progn
      (setq target-point (opor-2d target-point))
      (setq distance
        (opor-write-level-vba-distance reference-point target-point axis-angle))
      (if (numberp distance)
        (progn
          (setq delta
            (opor-write-level-vba-round (/ (* slope-percent distance) 100.0)))
          (setq new-value (+ reference-value delta))
          (setq text (opor-write-level-prefixed-text new-value plus-p))
          (setq result
            (vl-catch-all-apply 'opor-support-set-first-attribute (list block text)))
          (if (vl-catch-all-error-p result)
            nil
            (progn
              (opor-log
                (strcat
                  "h: расстояние=" (rtos distance 2 12)
                  ", delta=" (itoa delta)
                  ", отметка=" text "."))
              (list distance delta new-value text))))))))

(defun opor-write-level-run
  (/ reference slope reference-text reference-value plus-p slope-percent
     reference-point axis-angle picked block done count result prompt)
  (cond
    ((not (opor-import-level-block))
      (opor-alert
        "Не найден блок отметки otmetka_oporvb и не удалось загрузить его из библиотеки.")
      nil)
    ((not (opor-block-exists-p "slope"))
      (opor-alert "Не найден блок уклона slope.")
      nil)
    (t
      (setq reference
        (opor-write-level-pick-block
          "\nУкажите блок отметки высот воронки: "
          *opor-level-block-name*
          "Это не блок отметки высот воронки."))
      (if (not reference)
        nil
        (progn
          (setq slope
            (opor-write-level-pick-block
              "\nУкажите блок уклона: "
              "slope"
              "Это не блок уклона."))
          (if (not slope)
            nil
            (progn
              (setq reference-text (opor-first-attribute-text reference))
              (setq reference-value (opor-parse-real reference-text nil))
              (setq plus-p (and reference-text (vl-string-search "+" reference-text)))
              (setq slope-percent (opor-write-level-percent slope))
              (setq reference-point (opor-slope-insertion-point reference))
              (cond
                ((not (numberp reference-value))
                  (opor-alert "Не удалось прочитать опорную отметку.")
                  nil)
                ((not (numberp slope-percent))
                  (opor-alert "Не удалось прочитать процент уклона.")
                  nil)
                ((not reference-point)
                  (opor-alert "Не удалось прочитать точку опорной отметки.")
                  nil)
                (t
                  (setq reference-point (opor-2d reference-point))
                  (setq axis-angle (opor-write-level-axis-angle slope))
                  (setq count 0 done nil)
                  (while (not done)
                    (setq prompt
                      (if (= count 0)
                        "\nУкажите блок отметки высот для расчёта или Enter для завершения: "
                        "\nУкажите следующий блок отметки высот или Enter для завершения: "))
                    (setq picked (entsel prompt))
                    (if (not picked)
                      (setq done T)
                      (progn
                        (setq block (vlax-ename->vla-object (car picked)))
                        (if (not (opor-write-level-block-p block *opor-level-block-name*))
                          (opor-alert "Это не блок отметки высот.")
                          (progn
                            (setq result
                              (opor-write-level-update-one
                                reference-point reference-value plus-p slope-percent
                                axis-angle block))
                            (if result
                              (setq count (1+ count))
                              (opor-alert "Не удалось записать отметку.")))))))
                  (opor-session-set 'write-level-count count)
                  (opor-session-set 'write-level-percent slope-percent)
                  (opor-log
                    (strcat
                      "h завершён: отметок=" (itoa count)
                      ", база=" (opor-write-level-prefixed-text reference-value plus-p)
                      ", уклон=" (rtos slope-percent 2 2) "%."))
                  T)))))))))

(defun opor-command-write-level ()
  (opor-init-session)
  (opor-write-level-run))

(princ)
