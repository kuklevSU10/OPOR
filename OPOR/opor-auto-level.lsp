;;; OPOR +0.000/auto_levl: вставка пустых отметок во все уникальные вершины областей.

(defun opor-auto-level-pick-boundary (/ state picked obj)
  (setq state (opor-slope-layer-state *opor-layer-level-lines*))
  (opor-slope-layer-set-on *opor-layer-level-lines* nil)
  (setq picked
    (vl-catch-all-apply 'entsel (list "\nУкажите внешний контур: ")))
  (opor-slope-layer-put-state state)
  (if (vl-catch-all-error-p picked)
    nil
    (if picked
    (progn
      (setq obj (vlax-ename->vla-object (car picked)))
      (if (opor-polyline-object-p obj)
        obj
        (progn (opor-alert "Это не полилиния.") nil)))
      nil)))

(defun opor-auto-level-polylines (boundary / objects result obj)
  (setq objects (opor-level-read-polylines boundary))
  (setq result '())
  (foreach obj objects
    ;; VBA filter: DXF 0 = LWPOLYLINE, то есть AcDbPolyline.
    (if (= (opor-obj-name obj) "AcDbPolyline")
      (setq result (cons obj result))))
  (reverse result))

(defun opor-auto-level-unique-points (points / result pt)
  (setq result '())
  (foreach pt points
    (if (not (opor-point-near-any-p pt result 1.0))
      (setq result (append result (list (opor-2d pt))))))
  result)

(defun opor-auto-level-insert (pt / value block)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (opor-ms) (vlax-3d-point pt) *opor-level-block-name*
        1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq block value)
      (opor-register-created block "auto-level-mark"))))

(defun opor-auto-level-run (/ boundary polylines vertices points count block pline pt)
  (cond
    ((not (opor-layer-exists-p *opor-layer-level-lines*))
      (opor-alert "Не найден слой линии_высот.")
      nil)
    ((not (opor-import-level-block))
      (opor-alert
        "Не найден блок отметки otmetka_oporvb и не удалось загрузить его из библиотеки.")
      nil)
    (t
      (opor-view-save)
      (setq boundary (opor-auto-level-pick-boundary))
      (if (not boundary)
        nil
        (progn
          (opor-zoom-to-boundary boundary)
          (setq polylines (opor-auto-level-polylines boundary))
          (if (not polylines)
            (progn
              (opor-alert "Не найдены области высот.")
              nil)
            (progn
              (setq vertices '())
              (foreach pline polylines
                (setq vertices (append vertices (opor-polyline-vertices pline))))
              (setq points (opor-auto-level-unique-points vertices))
              (setq count 0)
              (foreach pt points
                (setq block (opor-auto-level-insert pt))
                (if block (setq count (1+ count))))
              (opor-session-set 'auto-level-polyline-count (length polylines))
              (opor-session-set 'auto-level-mark-count count)
              (opor-log
                (strcat
                  "+0.000 завершён: областей=" (itoa (length polylines))
                  ", отметок=" (itoa count) "."))
              (opor-alert (strcat "Проставлено " (itoa count) " отметок."))
              T)))))))

(defun opor-command-auto-level ()
  (opor-init-session)
  (opor-auto-level-run))

(princ)
