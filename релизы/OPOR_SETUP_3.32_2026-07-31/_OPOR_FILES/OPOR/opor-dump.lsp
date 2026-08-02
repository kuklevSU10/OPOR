;;; OPOR diagnostics for objects created by the clean LISP port.

(defun opor-object-xdata-info (obj / en data app values)
  (setq en (vlax-vla-object->ename obj))
  (if en
    (progn
      (setq data (entget en (list *opor-xdata-app*)))
      (setq app (cadr (assoc -3 data)))
      (if (and app (= (car app) *opor-xdata-app*))
        (progn
          (setq values
            (vl-remove-if-not
              '(lambda (item) (and (listp item) (= (car item) 1000)))
              (cdr app)))
          (list
            (cons 'type (if (nth 0 values) (cdr (nth 0 values)) "unknown"))
            (cons 'version (if (nth 1 values) (cdr (nth 1 values)) "unknown"))
            (cons 'session (if (nth 2 values) (cdr (nth 2 values)) "legacy"))))
        nil))
    nil))

(defun opor-object-xdata-type (obj / info)
  (setq info (opor-object-xdata-info obj))
  (if info (cdr (assoc 'type info)) nil))

(defun opor-dump-inc (key table / item)
  (setq item (assoc key table))
  (if item
    (subst (cons key (1+ (cdr item))) item table)
    (cons (cons key 1) table)))

(defun opor-dump-block-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun opor-dump-session-real (key / value)
  (setq value (opor-session-get key))
  (if (numberp value) value 0.0))

(defun opor-dump-session-int (key / value)
  (setq value (opor-session-get key))
  (if (numberp value) (fix value) 0))

(defun opor-dump-attributes (block / raw atts text)
  (setq text "")
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (opor-variant-list raw))
      (foreach att atts
        (setq text
          (strcat
            text
            "\n      "
            (vla-get-TagString att)
            " = "
            (vla-get-TextString att))))))
  text)

(defun opor-dump-session-ids (/ obj info sid sessions)
  (setq sessions '())
  (vlax-for obj (opor-ms)
    (setq info (opor-object-xdata-info obj))
    (if info
      (progn
        (setq sid (cdr (assoc 'session info)))
        (if (not (member sid sessions))
          (setq sessions (cons sid sessions))))))
  (vl-sort sessions '<))

;; S4: сводка плитки; пустая строка вне режима "p"
(defun opor-dump-tiles-text ()
  (if (= (opor-session-get 'tile-mode) "p")
    (strcat
      "\n    П8 плитка: размер="
      (rtos (opor-session-get 'tile-size-x) 2 0) "×"
      (rtos (opor-session-get 'tile-size-y) 2 0)
      ", шаг опор="
      (rtos (opor-session-get 'step-x) 2 0) "×"
      (rtos (opor-session-get 'step-y) 2 0)
      ", целых=" (itoa (opor-tiles-qc))
      ", обрезанных=" (itoa (opor-tiles-qr)))
    ""))

;; S6/P9: старый ручной шаг или новая раскладка по длине доски.
(defun opor-dump-dbl-lag-text (/ step layout layout-text)
  (setq step (opor-session-get 'double-lag-step))
  (setq layout (opor-session-get 'double-lag-layout))
  (if (and (numberp step) (> step 0.0))
    (if (member layout '("even" "half"))
      (progn
        (setq layout-text (if (= layout "half") "сдвиг 1/2" "ровно"))
        (strcat
          "\n    П9 сдвоенные лаги: доска="
          (rtos (opor-session-get 'board-length) 2 0)
          ", раскладка=" layout-text
          ", стык через=" (rtos step 2 0)
          ", сегментов пары=" (itoa (opor-dump-session-int 'dbl-lag-pair-segments))
          ", заменено обычных=" (itoa (opor-dump-session-int 'dbl-lag-deduped))
          ", шахматных опор=" (itoa (opor-dump-session-int 'dbl-lag-stagger-support-count))))
      (strcat
        "\n    S6 сдв. лаги: шаг=" (rtos step 2 0)
        ", создано=" (itoa (opor-dump-session-int 'dbl-lag-created))
        ", дедуп=" (itoa (opor-dump-session-int 'dbl-lag-deduped))))
    ""))

(defun opor-dump-one-session (session-id / obj info xtype sid oname layer len type-count layer-count grid-count grid-v-count grid-p-count grid-length grid-v-length grid-p-length grid-v-min grid-p-min support-count color-count table-count tables)
  (setq type-count '())
  (setq layer-count '())
  (setq color-count '())
  (setq tables '())
  (setq grid-count 0)
  (setq grid-v-count 0)
  (setq grid-p-count 0)
  (setq grid-length 0.0)
  (setq grid-v-length 0.0)
  (setq grid-p-length 0.0)
  (setq grid-v-min nil)
  (setq grid-p-min nil)
  (setq support-count 0)
  (setq table-count 0)
  (vlax-for obj (opor-ms)
    (setq info (opor-object-xdata-info obj))
    (if info
      (progn
        (setq sid (cdr (assoc 'session info)))
        (if (= sid session-id)
          (progn
            (setq xtype (cdr (assoc 'type info)))
            (setq oname (opor-obj-name obj))
            (setq layer (vla-get-Layer obj))
            (setq type-count (opor-dump-inc xtype type-count))
            (setq layer-count (opor-dump-inc layer layer-count))
            (cond
              ((and (= oname "AcDbLine") (wcmatch xtype "grid*"))
                (setq len (vla-get-Length obj))
                (setq grid-count (1+ grid-count))
                (setq grid-length (+ grid-length len))
                (if (= xtype "grid-v")
                  (progn
                    (setq grid-v-count (1+ grid-v-count))
                    (setq grid-v-length (+ grid-v-length len))
                    (if (or (not grid-v-min) (< len grid-v-min)) (setq grid-v-min len))))
                (if (= xtype "grid-p")
                  (progn
                    (setq grid-p-count (1+ grid-p-count))
                    (setq grid-p-length (+ grid-p-length len))
                    (if (or (not grid-p-min) (< len grid-p-min)) (setq grid-p-min len)))))
              ((= xtype "support")
                (setq support-count (1+ support-count))
                (setq color-count (opor-dump-inc (vla-get-Color obj) color-count)))
              ((wcmatch xtype "table*")
                (setq table-count (1+ table-count))
                (setq tables (cons obj tables)))))))))
  (princ (strcat "\n\n===== SESSION " session-id " ====="))
  (princ "\n--- ПО ТИПАМ XDATA ---")
  (foreach item (vl-sort type-count '(lambda (a b) (< (car a) (car b))))
    (princ (strcat "\n    " (car item) " : " (itoa (cdr item)))))
  (if (null type-count) (princ "\n    нет объектов с XData OPOR"))
  (princ "\n--- ПО СЛОЯМ ---")
  (foreach item (vl-sort layer-count '(lambda (a b) (< (car a) (car b))))
    (princ (strcat "\n    " (car item) " : " (itoa (cdr item)))))
  (princ
    (strcat
      "\n--- НОВАЯ СЕТКА --- линий: "
      (itoa grid-count)
      "  длина: "
      (rtos grid-length 2 1)
      " мм = "
      (rtos (/ grid-length 1000.0) 2 2)
      " м"))
  (princ
    (strcat
      "\n    grid-v: "
      (itoa grid-v-count)
      " линий, "
      (rtos (/ grid-v-length 1000.0) 2 2)
      " м"
      ", min "
      (if grid-v-min (rtos grid-v-min 2 1) "-")
      " мм"
      "\n    grid-p: "
      (itoa grid-p-count)
      " линий, "
      (rtos (/ grid-p-length 1000.0) 2 2)
      " м"
      ", min "
      (if grid-p-min (rtos grid-p-min 2 1) "-")
      " мм"))
  (if (and (boundp '*opor-session*)
           *opor-session*
           (= session-id (opor-session-get 'session-id)))
    (princ
      (strcat
        "\n    session grid lag: "
        (rtos (/ (opor-dump-session-real 'grid-lag-length-mm) 1000.0) 2 2)
        " м"
        "\n    index ranges"
        (if (> (opor-dump-session-int 'contour-count) 1) " (последний контур)" "")
        ": v="
        (vl-princ-to-string (opor-session-get 'grid-v-index-range))
        " p="
        (vl-princ-to-string (opor-session-get 'grid-p-index-range))
        "\n    boundary lag candidate: "
        (rtos (/ (opor-dump-session-real 'boundary-lag-length-mm) 1000.0) 2 2)
        " м, mode="
        *opor-boundary-lag-length-mode*
        "\n      outer: "
        (rtos (/ (opor-dump-session-real 'boundary-outer-lag-length-mm) 1000.0) 2 2)
        " м, holes: "
        (rtos (/ (opor-dump-session-real 'boundary-holes-lag-length-mm) 1000.0) 2 2)
        " м"
        "\n    П5 лаги: рядов="
        (itoa (opor-dump-session-int 'lag-row-count))
        ", поперечных удалено="
        (itoa (opor-dump-session-int 'perp-lines-removed))
        (opor-dump-dbl-lag-text)
        (opor-dump-tiles-text)
        "\n    support split: vertices="
        (itoa (opor-dump-session-int 'support-vertex-count))
        " border="
        (itoa (opor-dump-session-int 'support-border-count))
        " nodes="
        (itoa (opor-dump-session-int 'support-node-count))
        "\n    border filter: raw="
        (itoa (opor-dump-session-int 'support-raw-border-count))
        " after-vertices="
        (itoa (opor-dump-session-int 'support-border-count))
        "\n    node filter: raw="
        (itoa (opor-dump-session-int 'support-raw-node-count))
        " after-self="
        (itoa (opor-dump-session-int 'support-after-self-node-count))
        " after-vertices="
        (itoa (opor-dump-session-int 'support-after-vertex-node-count))
        " after-border="
        (itoa (opor-dump-session-int 'support-node-count))
        "\n    крепёж: лаги="
        (opor-string (opor-session-get 'lag-fastener))
        " x"
        (itoa (opor-dump-session-int 'lag-fastener-count))
        ", плитки="
        (opor-string (opor-session-get 'tile-fastener))
        " x"
        (itoa (opor-dump-session-int 'tile-fastener-count))
        " (шаг "
        (rtos (opor-dump-session-real 'tile-fastener-step) 2 0)
        ")"
        "\n    levels: marks="
        (itoa (opor-dump-session-int 'level-mark-count))
        " polylines="
        (itoa (opor-dump-session-int 'level-polyline-count))
        " vertices="
        (itoa (opor-dump-session-int 'level-vertex-count))
        " missing-marks="
        (itoa (opor-dump-session-int 'level-missing-mark-count))
        " triangles="
        (itoa (opor-dump-session-int 'level-triangle-count))
        " height-errors="
        (itoa (opor-dump-session-int 'support-height-errors)))))
  (princ (strcat "\n--- НОВЫЕ ОПОРЫ --- всего: " (itoa support-count)))
  (foreach item (vl-sort color-count '(lambda (a b) (< (car a) (car b))))
    (princ (strcat "\n    цвет " (itoa (car item)) " : " (itoa (cdr item)) " шт")))
  (princ (strcat "\n--- НОВЫЕ ТАБЛИЦЫ --- всего: " (itoa table-count)))
  (foreach table tables
    (princ (strcat "\n    блок: " (opor-dump-block-name table)))
    (princ (opor-dump-attributes table)))
  (princ))

(defun opor-dump-created (/ sessions)
  (setq sessions (opor-dump-session-ids))
  (princ "\n========== OPOR NEW OBJECT DUMP ==========")
  (if sessions
    (foreach session-id sessions
      (opor-dump-one-session session-id))
    (princ "\nНет объектов с XData OPOR."))
  (princ "\n=========================================")
  (princ))

(defun opor-total-table-block-p (obj / name)
  (and (= (opor-obj-name obj) "AcDbBlockReference")
       (progn
         (setq name (opor-dump-block-name obj))
         (or
           (wcmatch name "table_totl_*")
           (member name
             (list *opor-new-3d-support-table-block*
                   *opor-new-pro-support-table-block*
                   *opor-new-tile-params-block*
                   *opor-new-board-params-block*
                   *opor-new-extra-row-block*))))))

(defun opor-dump-all-total-tables (/ obj info session count)
  (setq count 0)
  (princ "\n========== OPOR TOTAL TABLE BLOCKS ==========")
  (vlax-for obj (opor-ms)
    (if (opor-total-table-block-p obj)
      (progn
        (setq count (1+ count))
        (setq info (opor-object-xdata-info obj))
        (setq session (if info (cdr (assoc 'session info)) "old/no-xdata"))
        (princ
          (strcat
            "\n--- TABLE #"
            (itoa count)
            " ---"
            "\n    block: "
            (opor-dump-block-name obj)
            "\n    handle: "
            (vla-get-Handle obj)
            "\n    layer: "
            (vla-get-Layer obj)
            "\n    session: "
            session))
        (princ (opor-dump-attributes obj)))))
  (if (= count 0)
    (princ "\nНе найдено итоговых блоков таблиц."))
  (princ "\n============================================")
  (princ))

;; --- OPOROLD: снимок старых VBA-объектов внутри контура текущей сессии ---

(defun opor-insertion-point (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p value)
    nil
    (opor-2d (vlax-safearray->list (vlax-variant-value value)))))

(defun opor-old-support-block-name-p (name line / expected)
  (setq expected (if line (opor-support-block-name line) nil))
  (if expected
    (= name expected)
    (member name (mapcar 'cdr *opor-block-by-line*))))

(defun opor-old-grid-line-p (obj)
  (and
    (= (opor-obj-name obj) "AcDbLine")
    (= (vla-get-Layer obj) *opor-layer-grid*)
    (not (opor-object-has-opor-xdata-p obj))))

(defun opor-midpoint (a b)
  (list
    (/ (+ (car a) (car b)) 2.0)
    (/ (+ (cadr a) (cadr b)) 2.0)
    0.0))

(defun opor-grid-line-class (obj vec perp / a b dir)
  (setq a (opor-curve-start obj))
  (setq b (opor-curve-end obj))
  (setq dir (opor-v- b a))
  (cond
    ((opor-parallel-p dir vec) "grid-v")
    ((opor-parallel-p dir perp) "grid-p")
    (t "other")))

(defun opor-dump-old-near-session (/ boundary holes line supports obj name pt color attr height support index row-count color-count total old-grid-count old-grid-length old-v-count old-v-length old-p-count old-p-length old-other-count old-other-length vec perp class len mid)
  (if (not (and (boundp '*opor-session*) *opor-session* (opor-session-get 'outer-boundary)))
    (progn
      (princ "\nOPOROLD: сначала запусти OPOR, чтобы была текущая сессия с контуром.")
      (princ))
    (progn
      (setq boundary (opor-session-get 'outer-boundary))
      (setq holes (opor-session-get 'holes))
      (setq line (opor-session-get 'line))
      (setq supports (opor-read-supports line))
      (setq row-count '())
      (setq color-count '())
      (setq total 0)
      (setq old-grid-count 0)
      (setq old-grid-length 0.0)
      (setq old-v-count 0)
      (setq old-v-length 0.0)
      (setq old-p-count 0)
      (setq old-p-length 0.0)
      (setq old-other-count 0)
      (setq old-other-length 0.0)
      (setq vec (opor-unit (opor-v- (opor-session-get 'direction-point) (opor-session-get 'base-point))))
      (setq perp (if vec (opor-perp2d vec) nil))
      (vlax-for obj (opor-ms)
        (cond
          ((and
             (= (opor-obj-name obj) "AcDbBlockReference")
             (not (opor-object-has-opor-xdata-p obj))
             (opor-old-support-block-name-p (opor-dump-block-name obj) line)
             (setq pt (opor-insertion-point obj))
             (opor-point-in-working-area-p pt boundary holes))
            (setq total (1+ total))
            (setq color (vla-get-Color obj))
            (setq color-count (opor-dump-inc color color-count))
            (setq attr (opor-first-attribute-text obj))
            (setq height (opor-parse-real attr nil))
            (setq support (if height (opor-support-for-height height supports) nil))
            (if support
              (progn
                (setq index (cdr (assoc 'index support)))
                (setq row-count (opor-inc-index-count index row-count)))))
          ((and
             vec
             perp
             (opor-old-grid-line-p obj)
             (setq mid (opor-midpoint (opor-curve-start obj) (opor-curve-end obj)))
             (opor-point-in-working-area-p mid boundary holes))
            (setq len (vla-get-Length obj))
            (setq old-grid-count (1+ old-grid-count))
            (setq old-grid-length (+ old-grid-length len))
            (setq class (opor-grid-line-class obj vec perp))
            (cond
              ((= class "grid-v")
                (setq old-v-count (1+ old-v-count))
                (setq old-v-length (+ old-v-length len)))
              ((= class "grid-p")
                (setq old-p-count (1+ old-p-count))
                (setq old-p-length (+ old-p-length len)))
              (t
                (setq old-other-count (1+ old-other-count))
                (setq old-other-length (+ old-other-length len)))))))
      (princ "\n========== OPOR OLD OBJECTS NEAR CURRENT SESSION ==========")
      (princ (strcat "\nline: " (opor-string line)))
      (princ (strcat "\n--- OLD SUPPORT BLOCKS --- total: " (itoa total)))
      (foreach item (vl-sort color-count '(lambda (a b) (< (car a) (car b))))
        (princ (strcat "\n    color " (itoa (car item)) " : " (itoa (cdr item)))))
      (princ "\n--- OLD SUPPORTS BY HEIGHT RANGE ---")
      (foreach item (vl-sort row-count '(lambda (a b) (< (car a) (car b))))
        (princ (strcat "\n    " (itoa (car item)) " = " (itoa (cdr item)))))
      (princ
        (strcat
          "\n--- OLD GRID --- lines: "
          (itoa old-grid-count)
          " length: "
          (rtos (/ old-grid-length 1000.0) 2 2)
          " m"
          "\n    grid-v: "
          (itoa old-v-count)
          " lines, "
          (rtos (/ old-v-length 1000.0) 2 2)
          " m"
          "\n    grid-p: "
          (itoa old-p-count)
          " lines, "
          (rtos (/ old-p-length 1000.0) 2 2)
          " m"
          "\n    other: "
          (itoa old-other-count)
          " lines, "
          (rtos (/ old-other-length 1000.0) 2 2)
          " m"))
      (princ "\n===========================================================")
      (princ))))

(defun opor-show-handle (/ handle en obj bbox ll ur)
  (setq handle (getstring T "\nHandle объекта для показа: "))
  (if (= handle "")
    (princ "\nHandle не указан.")
    (progn
      (setq en (handent handle))
      (if (not en)
        (princ (strcat "\nОбъект с handle " handle " не найден."))
        (progn
          (setq obj (vlax-ename->vla-object en))
          (redraw en 3)
          (setq bbox (opor-bbox obj))
          (if bbox
            (progn
              (setq ll (car bbox))
              (setq ur (cadr bbox))
              (command "_.ZOOM" "_W" ll ur))
            (command "_.ZOOM" "_O" en ""))
          (princ
            (strcat
              "\nПоказан объект "
              handle
              " / "
              (opor-obj-name obj)
              " / слой "
              (vla-get-Layer obj)))))))
  (princ))

(princ)
