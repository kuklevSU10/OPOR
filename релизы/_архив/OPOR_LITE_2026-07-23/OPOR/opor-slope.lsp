;;; OPOR slopeWR + slope: порт VBA jj_slopeWR/j_slope/v_read_blk_slope.
;;; Материальная семантика 1:1: чужие опоры перекрашиваются, при <2% удаляются.

(defun opor-slope-layer-object (name / value)
  (setq value
    (vl-catch-all-apply
      'vla-Item
      (list (vla-get-Layers (opor-doc)) name)))
  (if (vl-catch-all-error-p value) nil value))

(defun opor-slope-layer-state (name / layer state)
  (setq layer (opor-slope-layer-object name))
  (if layer
    (progn
      (setq state (vl-catch-all-apply 'vla-get-LayerOn (list layer)))
      (if (vl-catch-all-error-p state) nil (list layer state)))
    nil))

(defun opor-slope-layer-put-state (state)
  (if state
    (vl-catch-all-apply 'vla-put-LayerOn (list (car state) (cadr state))))
  (princ))

(defun opor-slope-layer-set-on (name on / layer)
  (setq layer (opor-slope-layer-object name))
  (if layer
    (vl-catch-all-apply
      'vla-put-LayerOn
      (list layer (if on :vlax-true :vlax-false))))
  layer)

;; VBA гасит линии_высот (и для slope также плиткаvb) на время выбора.
;; Порт возвращает прежнее состояние сразу после entsel.
(defun opor-slope-pick-boundary (next-p hide-tiles-p / states state prompt picked obj)
  (setq states '())
  (setq state (opor-slope-layer-state *opor-layer-level-lines*))
  (if state (setq states (cons state states)))
  (if hide-tiles-p
    (progn
      (setq state (opor-slope-layer-state *opor-layer-tiles*))
      (if state (setq states (cons state states)))))
  (foreach state states
    (vl-catch-all-apply 'vla-put-LayerOn (list (car state) :vlax-false)))
  (setq prompt
    (if next-p
      "\nУкажите следующий внешний контур или Enter для завершения: "
      "\nУкажите внешний контур или Enter для завершения: "))
  (setq picked (vl-catch-all-apply 'entsel (list prompt)))
  (foreach state states (opor-slope-layer-put-state state))
  (cond
    ((vl-catch-all-error-p picked) nil)
    ((not picked) nil)
    (t
      (setq obj (opor-to-vla (car picked)))
      (if (opor-polyline-object-p obj)
        obj
        (progn
          (opor-alert "Это не полилиния.")
          'invalid)))))

(defun opor-slope-insertion-point (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p value)
    nil
    (opor-variant-list value)))

;; VBA принудительно опускает блоки slope/отметок/опор на Z=0.
(defun opor-slope-flatten-block (obj / pt)
  (setq pt (opor-slope-insertion-point obj))
  (if pt
    (vl-catch-all-apply
      'vla-put-InsertionPoint
      (list obj (vlax-3d-point (opor-2d pt)))))
  (if pt (opor-2d pt) nil))

(defun opor-slope-block-name-p (obj name)
  (and
    (= (opor-obj-name obj) "AcDbBlockReference")
    (= (strcase (opor-effective-block-name obj)) (strcase name))))

(defun opor-slope-support-block-p (obj / name)
  (setq name (strcase (opor-effective-block-name obj)))
  (member name
    (mapcar 'strcase '("opor_symb" "opor_symb3d" "opor_symbPRO"))))

;; VBA SelectionSet.Select видит INSERT на выключенных слоях, а ssget "_C" —
;; нет. Поэтому блоки читаем прямо из ModelSpace и проверяем пересечение bbox
;; по extents объекта: результат не зависит от экранного состояния слоя.
(defun opor-slope-bboxes-overlap-p (a b / all aur bll bur)
  (setq all (car a) aur (cadr a) bll (car b) bur (cadr b))
  (and
    (<= (car all) (car bur))
    (<= (car bll) (car aur))
    (<= (cadr all) (cadr bur))
    (<= (cadr bll) (cadr aur))))

(defun opor-slope-crossing-blocks (bbox / result obj object-bbox)
  (setq result '())
  (vlax-for obj (opor-ms)
    (if (= (opor-obj-name obj) "AcDbBlockReference")
      (progn
        (setq object-bbox (opor-bbox obj))
        (if (and object-bbox (opor-slope-bboxes-overlap-p bbox object-bbox))
          (setq result (cons obj result))))))
  (reverse result))

(defun opor-slope-crossing-areas (bbox)
  (opor-ssget-crossing-objects
    bbox
    (list
      (cons 0 "LWPOLYLINE,POLYLINE")
      (cons 8 *opor-layer-level-lines*))))

(defun opor-slope-filter-blocks (objects name / result)
  (setq result '())
  (foreach obj objects
    (if (opor-slope-block-name-p obj name)
      (setq result (cons obj result))))
  (reverse result))

(defun opor-slope-filter-supports (objects / result)
  (setq result '())
  (foreach obj objects
    (if (opor-slope-support-block-p obj)
      (progn
        (opor-slope-flatten-block obj)
        (setq result (cons obj result)))))
  (reverse result))

(defun opor-slope-record-get (key record)
  (cdr (assoc key record)))

(defun opor-slope-number-from-block (block / text)
  (setq text (opor-first-attribute-text block))
  (if text
    (opor-parse-real (opor-string-replace-all "%" "" text) nil)
    nil))

(defun opor-slope-prepare-records (objects / records obj pt raw rounded text)
  (setq records '())
  (foreach obj objects
    (setq pt (opor-slope-flatten-block obj))
    (setq raw (opor-slope-number-from-block obj))
    (setq rounded (if (numberp raw) (opor-round-half-even raw) nil))
    (setq text (if (numberp rounded) (strcat (itoa rounded) "%") nil))
    ;; j_slope обновляет округлённый текст только для фактического уклона >=2.
    (if (and (numberp raw) (>= raw *opor-slope-min-percent*))
      (opor-support-set-first-attribute obj text))
    (setq records
      (cons
        (list
          (cons 'object obj)
          (cons 'point pt)
          (cons 'raw raw)
          (cons 'rounded rounded)
          (cons 'text text))
        records)))
  (reverse records))

(defun opor-slope-read-marks (objects / records obj pt text height)
  (setq records '())
  (foreach obj objects
    (if (opor-slope-block-name-p obj *opor-level-block-name*)
      (progn
        (setq pt (opor-slope-flatten-block obj))
        (setq text (opor-first-attribute-text obj))
        (setq height (opor-parse-real text nil))
        (setq records
          (cons
            (list
              (cons 'object obj)
              (cons 'point pt)
              (cons 'height height))
            records)))))
  (reverse records))

(defun opor-slope-record-inside-area (records area / found pt)
  (setq found nil)
  (foreach record records
    (if (not found)
      (progn
        (setq pt (opor-slope-record-get 'point record))
        (if (and pt (opor-point-inside-boundary-p pt area))
          (setq found record)))))
  found)

(defun opor-slope-support-in-area-p (support area / pt)
  (setq pt (opor-slope-insertion-point support))
  (and pt
    (or
      (opor-point-inside-boundary-p (opor-2d pt) area)
      (opor-point-on-curve-p
        (opor-2d pt) area *opor-slope-boundary-tolerance*))))

(defun opor-slope-mark-height-near (marks pt / found height mp)
  (setq found nil)
  (foreach mark marks
    (if (not found)
      (progn
        (setq height (opor-slope-record-get 'height mark))
        (setq mp (opor-slope-record-get 'point mark))
        (if (and (numberp height) mp
                 (<= (distance (opor-2d pt) (opor-2d mp))
                     *opor-slope-mark-tolerance*))
          (setq found height)))))
  found)

;; Возвращает (min max pmin pmax missing-count), как get_levls после Explode.
;; Для полилиний с дуговыми сегментами нужны именно вершины/концы сегментов.
(defun opor-slope-area-minmax (area marks / levmin levmax pmin pmax missing height)
  (setq levmin 999999.0)
  (setq levmax -999999.0)
  (setq pmin nil)
  (setq pmax nil)
  (setq missing 0)
  (foreach pt (opor-polyline-vertices area)
    (setq pt (opor-2d pt))
    (setq height (opor-slope-mark-height-near marks pt))
    (if (numberp height)
      (progn
        (if (< height levmin) (setq levmin height pmin pt))
        (if (> height levmax) (setq levmax height pmax pt)))
      (progn
        (opor-error-circle pt)
        (setq missing (1+ missing)))))
  (list levmin levmax pmin pmax missing))

(defun opor-slope-write-area (area slope-record marks / mm levmin levmax pmin pmax dist value block)
  (setq mm (opor-slope-area-minmax area marks))
  (setq levmin (nth 0 mm))
  (setq levmax (nth 1 mm))
  (setq pmin (nth 2 mm))
  (setq pmax (nth 3 mm))
  (setq block (opor-slope-record-get 'object slope-record))
  (if (and pmin pmax (> levmax levmin))
    (progn
      (setq dist (distance pmin pmax))
      (setq value
        (if (> dist 1e-9)
          (opor-round-half-even (* (/ (- levmax levmin) dist) 100.0))
          0)))
    (setq value 0))
  (opor-support-set-first-attribute block (strcat (itoa value) "%"))
  (if (< value *opor-slope-min-percent*)
    (vl-catch-all-apply 'vla-put-Layer (list block *opor-layer-support-text*)))
  (if (> value *opor-slope-max-percent*)
    (vl-catch-all-apply 'vla-put-Color (list block 1)))
  (list value (nth 4 mm)))

(defun opor-slope-write-run (/ boundary bbox blocks slopes marks areas processed errmin errmax missing record result value)
  (if (not (opor-block-exists-p "slope"))
    (progn (opor-alert "Не найден блок уклона slope.") nil)
    (progn
      (opor-ensure-layer *opor-layer-support-text* 9 "Continuous")
      ;; VBA оставляет слой ошибок выключенным.
      (opor-slope-layer-set-on *opor-layer-support-text* nil)
      (opor-view-save)
      (setq boundary (opor-slope-pick-boundary nil nil))
      (cond
        ((or (not boundary) (eq boundary 'invalid)) nil)
        (t
          (opor-zoom-to-boundary boundary)
          (setq bbox (opor-bbox boundary))
          (setq blocks (opor-slope-crossing-blocks bbox))
          (setq slopes
            (opor-slope-prepare-records
              (opor-slope-filter-blocks blocks "slope")))
          (setq marks (opor-slope-read-marks blocks))
          (setq areas (opor-slope-crossing-areas bbox))
          (cond
            ((not areas) (opor-alert "Не найдены области высот.") nil)
            ((not slopes) (opor-alert "Не найдены блоки уклонов.") nil)
            ((not marks) (opor-alert "Не найдены блоки отметок.") nil)
            (t
              (setq processed 0 errmin 0 errmax 0 missing 0)
              (foreach area areas
                (setq record (opor-slope-record-inside-area slopes area))
                (if record
                  (progn
                    (setq result (opor-slope-write-area area record marks))
                    (setq value (car result))
                    (setq missing (+ missing (cadr result)))
                    (setq processed (1+ processed))
                    (if (< value *opor-slope-min-percent*)
                      (setq errmin (1+ errmin)))
                    (if (> value *opor-slope-max-percent*)
                      (setq errmax (1+ errmax))))))
              (opor-session-set 'slope-write-area-count processed)
              (opor-session-set 'slope-write-under-min errmin)
              (opor-session-set 'slope-write-over-max errmax)
              (opor-session-set 'slope-write-missing-marks missing)
              (if (or (> errmin 0) (> errmax 0))
                (opor-alert
                  (strcat
                    "Количество уклонов вне заданного диапазона:"
                    "\nсвыше 9%    " (itoa errmax)
                    "\nменьше 2%   " (itoa errmin)
                    "\n\nУклоны меньше 2% перенесены на слой "
                    *opor-layer-support-text* ".")))
              (opor-log
                (strcat
                  "Slope % завершён: областей=" (itoa processed)
                  ", меньше 2%=" (itoa errmin)
                  ", свыше 9%=" (itoa errmax)
                  ", ошибок отметок=" (itoa missing) "."))
              T)))))))

;; v_read_blk_slope: восемь окружностей table_slope сверху вниз задают P2..P9.
(defun opor-slope-color-map (/ blocks definition value circles center idx result item)
  (setq blocks (vla-get-Blocks (opor-doc)))
  (setq value (vl-catch-all-apply 'vla-Item (list blocks "table_slope")))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq definition value)
      (setq circles '())
      (vlax-for obj definition
        (if (= (opor-obj-name obj) "AcDbCircle")
          (progn
            (setq center
              (opor-variant-list
                (vl-catch-all-apply 'vla-get-Center (list obj))))
            (if center
              (setq circles
                (cons (cons (cadr center) (vla-get-Color obj)) circles))))))
      (setq circles
        (vl-sort circles '(lambda (a b) (> (car a) (car b)))))
      (if (< (length circles) 8)
        nil
        (progn
          (setq idx 2 result '())
          (while (and circles (<= idx 9))
            (setq item (car circles))
            (setq result (cons (cons idx (cdr item)) result))
            (setq circles (cdr circles))
            (setq idx (1+ idx)))
          (reverse result))))))

(defun opor-slope-counts-init ()
  '((2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0) (7 . 0) (8 . 0) (9 . 0)))

(defun opor-slope-count (percent counts / pair)
  (setq pair (assoc percent counts))
  (if pair (cdr pair) 0))

(defun opor-slope-count-inc (percent counts / pair)
  (setq pair (assoc percent counts))
  (if pair
    (subst (cons percent (1+ (cdr pair))) pair counts)
    counts))

(defun opor-slope-session-inc (key / value)
  (setq value (opor-session-get key))
  (if (not (numberp value)) (setq value 0))
  (opor-session-set key (1+ value)))

(defun opor-slope-process-contour (boundary colors counts / bbox blocks areas slopes supports remaining next record raw rounded color)
  (setq bbox (opor-bbox boundary))
  (opor-zoom-to-boundary boundary)
  (setq blocks (opor-slope-crossing-blocks bbox))
  (setq areas (opor-slope-crossing-areas bbox))
  (setq slopes
    (opor-slope-prepare-records
      (opor-slope-filter-blocks blocks "slope")))
  (setq supports (opor-slope-filter-supports blocks))
  (cond
    ((not slopes)
      (opor-alert "Не найдены блоки уклонов.")
      counts)
    ((not supports)
      (opor-alert "Не найдены блоки опор.")
      counts)
    ((not areas)
      (opor-alert "Не найдены области высот.")
      counts)
    (t
      (setq remaining supports)
      (foreach area areas
        (setq record (opor-slope-record-inside-area slopes area))
        (if record
          (progn
            (setq raw (opor-slope-record-get 'raw record))
            (setq rounded (opor-slope-record-get 'rounded record))
            (if (numberp raw)
              (progn
                (setq next '())
                (foreach support remaining
                  (if (opor-slope-support-in-area-p support area)
                    (if (>= raw *opor-slope-min-percent*)
                      (progn
                        ;; AutoLISP (and ...) возвращает T/nil, не последнее
                        ;; значение: номер ACI надо извлекать отдельным присваиванием.
                        (setq color nil)
                        (if (numberp rounded)
                          (setq color (cdr (assoc rounded colors))))
                        (if color
                          (vl-catch-all-apply 'vla-put-Color (list support color)))
                        (if (and (numberp rounded) (>= rounded 2) (<= rounded 9))
                          (setq counts (opor-slope-count-inc rounded counts)))
                        (opor-slope-session-inc 'slope-support-assigned))
                      (progn
                        (opor-delete-object support)
                        (opor-slope-session-inc 'slope-support-deleted)))
                    (setq next (cons support next))))
                (setq remaining (reverse next)))))))
      counts)))

(defun opor-slope-add-value-if-positive (tag value values)
  (if (> value 0) (cons (cons tag (itoa value)) values) values))

(defun opor-slope-table-values (counts / p2 p3 p4 p5 p6 p7 p8 p9 p2all p3all values)
  (setq p2 (opor-slope-count 2 counts))
  (setq p3 (opor-slope-count 3 counts))
  (setq p4 (opor-slope-count 4 counts))
  (setq p5 (opor-slope-count 5 counts))
  (setq p6 (opor-slope-count 6 counts))
  (setq p7 (opor-slope-count 7 counts))
  (setq p8 (opor-slope-count 8 counts))
  (setq p9 (opor-slope-count 9 counts))
  (setq p2all (+ p2 (* p4 2) p5 (* p7 2) p8))
  (setq p3all (+ p3 p5 (* p6 2) p7 (* p8 2) (* p9 3)))
  (setq values
    (list
      (cons "P2-ALL" (itoa p2all))
      (cons "P3-ALL" (itoa p3all))))
  (foreach pair counts
    (setq values
      (opor-slope-add-value-if-positive
        (strcat "P" (itoa (car pair))) (cdr pair) values)))
  (setq values (opor-slope-add-value-if-positive "P2-2" p2 values))
  (setq values (opor-slope-add-value-if-positive "P3-3" p3 values))
  (setq values (opor-slope-add-value-if-positive "P4-2" (* p4 2) values))
  (setq values (opor-slope-add-value-if-positive "P5-2" p5 values))
  (setq values (opor-slope-add-value-if-positive "P5-3" p5 values))
  (setq values (opor-slope-add-value-if-positive "P6-3" (* p6 2) values))
  (setq values (opor-slope-add-value-if-positive "P7-2" (* p7 2) values))
  (setq values (opor-slope-add-value-if-positive "P7-3" p7 values))
  (setq values (opor-slope-add-value-if-positive "P8-2" p8 values))
  (setq values (opor-slope-add-value-if-positive "P8-3" (* p8 2) values))
  (setq values (opor-slope-add-value-if-positive "P9-3" (* p9 3) values))
  values)

(defun opor-slope-insert-table (pt counts / value block)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (opor-ms) (vlax-3d-point pt) "table_slope"
        1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    (progn
      (opor-alert
        (strcat "Не удалось вставить table_slope:\n"
          (vl-catch-all-error-message value)))
      nil)
    (progn
      (setq block value)
      (opor-register-created block "slope-table")
      (opor-set-attribute-values block (opor-slope-table-values counts))
      block)))

(defun opor-slope-counts-text (counts / text)
  (setq text "")
  (foreach pair counts
    (setq text
      (strcat text
        (if (= text "") "" ", ")
        "P" (itoa (car pair)) "=" (itoa (cdr pair)))))
  text)

(defun opor-slope-run (/ colors counts boundary table-point qloop done aborted table)
  (cond
    ((not (opor-block-exists-p "slope"))
      (opor-alert "Не найден блок уклона slope.")
      nil)
    ((not (opor-block-exists-p "table_slope"))
      (opor-alert "Не найден блок table_slope.")
      nil)
    ((not (setq colors (opor-slope-color-map)))
      (opor-alert "В table_slope не найдены восемь цветовых окружностей P2...P9.")
      nil)
    (t
      (opor-view-save)
      (setq counts (opor-slope-counts-init))
      (opor-session-set 'slope-support-assigned 0)
      (opor-session-set 'slope-support-deleted 0)
      (setq qloop 0 done nil aborted nil table-point nil)
      (while (not done)
        (setq boundary (opor-slope-pick-boundary (> qloop 0) T))
        (cond
          ((not boundary) (setq done T))
          ((eq boundary 'invalid) (setq done T aborted T))
          (t
            (if (= qloop 0)
              (progn
                (setq table-point
                  (getpoint "\nУкажите точку верхнего левого угла таблицы: "))
                (if (not table-point) (setq done T aborted T))))
            (if (not done)
              (progn
                (setq counts (opor-slope-process-contour boundary colors counts))
                (setq qloop (1+ qloop)))))))
      (if (and (not aborted) (> qloop 0) table-point)
        (progn
          (setq table (opor-slope-insert-table (opor-2d table-point) counts))
          (opor-session-set 'slope-contour-count qloop)
          (opor-session-set 'slope-counts counts)
          (opor-log
            (strcat
              "Slope завершён: контуров=" (itoa qloop)
              ", назначено=" (itoa (opor-session-get 'slope-support-assigned))
              ", удалено=" (itoa (opor-session-get 'slope-support-deleted))
              ", " (opor-slope-counts-text counts) "."))
          (if table T nil))
        nil))))

;; Прямые команды loader и DCL-диспетчер используют одни и те же runners.
(defun opor-command-slope-write ()
  (opor-init-session)
  (opor-slope-write-run))

(defun opor-command-slope ()
  (opor-init-session)
  (opor-slope-run))

(princ)
