;;; OPOR core workflow, session and public commands

(setq *opor-session* nil)

(defun opor-new-session-id (/ cdate millis)
  (setq cdate (opor-safe-getvar "CDATE"))
  (setq millis (opor-safe-getvar "MILLISECS"))
  (strcat
    (vl-string-translate "." "_" (rtos (if cdate cdate 0.0) 2 8))
    "-"
    (itoa (if millis millis 0))))

(defun opor-init-session (/ pair)
  (setq *opor-session* '())
  (foreach pair *opor-default-params*
    (setq *opor-session* (cons pair *opor-session*)))
  (setq *opor-session* (cons (cons 'session-id (opor-new-session-id)) *opor-session*))
  (setq *opor-session* (cons (cons 'created-objects '()) *opor-session*))
  (setq *opor-session* (reverse *opor-session*)))

(defun opor-session-get (key)
  (cdr (assoc key *opor-session*)))

(defun opor-session-set (key value / pair)
  (setq pair (assoc key *opor-session*))
  (if pair
    (setq *opor-session* (subst (cons key value) pair *opor-session*))
    (setq *opor-session* (append *opor-session* (list (cons key value)))))
  value)

(defun opor-session-push (key value / old)
  (setq old (opor-session-get key))
  (if (not (listp old)) (setq old '()))
  (opor-session-set key (cons value old)))

(defun opor-register-created (obj object-type)
  (if obj
    (if (opor-mark-object obj object-type)
      (progn
        (opor-session-push 'created-objects obj)
        obj)
      (progn
        (opor-log
          (strcat "Объект " (opor-string object-type)
            " удалён: не удалось пометить его XData OPOR."))
        (opor-delete-object obj)
        nil))
    nil))

(defun opor-unregister-created (obj / objects)
  (setq objects (opor-session-get 'created-objects))
  (if objects
    (opor-session-set 'created-objects (vl-remove obj objects)))
  obj)

(defun opor-missing-layers (/ missing)
  (setq missing '())
  (foreach def *opor-layer-defs*
    (if (not (opor-layer-exists-p (car def)))
      (setq missing (cons (car def) missing))))
  (reverse missing))

(defun opor-missing-blocks (blocks / missing)
  (setq missing '())
  (foreach block blocks
    (if (not (opor-block-exists-p block))
      (setq missing (cons block missing))))
  (reverse missing))

(defun opor-dxf-set (data code value / old)
  (if (setq old (assoc code data))
    (subst (cons code value) old data)
    (append data (list (cons code value)))))

(defun opor-dxf-remove (data code)
  (vl-remove-if '(lambda (pair) (= (car pair) code)) data))

;; Built-in arrow blocks appear in the block table only after AutoCAD has used
;; them once. Create _OBLIQUE through DIMBLK, then restore the user's override.
(defun opor-ensure-oblique-arrow-block (/ old result block)
  (setq block (tblobjname "BLOCK" "_OBLIQUE"))
  (if (not block)
    (progn
      (setq old (opor-safe-getvar "DIMBLK"))
      (setq result (opor-safe-setvar "DIMBLK" "_OBLIQUE"))
      (setq block (tblobjname "BLOCK" "_OBLIQUE"))
      (if old
        (opor-safe-setvar "DIMBLK" (if (= old "") "." old)))))
  block)

(defun opor-dimstyle-apply-extended-settings (style-en / data result)
  ;; These two Dimension Style dialog values are stored as Autodesk XData,
  ;; not as the legacy DIMSTYLE table group codes returned by plain entget.
  (regapp "ACAD_DSTYLE_DIMJAG")
  (regapp "ACAD_DSTYLE_DIMTALN")
  (setq data
    (append
      (entget style-en)
      (list
        (list -3
          (list "ACAD_DSTYLE_DIMJAG"
            (cons 1070 388)
            (cons 1040 *opor-dimstyle-jog-height-factor*))
          (list "ACAD_DSTYLE_DIMTALN"
            (cons 1070 392)
            (cons 1070 *opor-dimstyle-text-alignment*))))))
  (setq result (vl-catch-all-apply 'entmod (list data)))
  (if (or (vl-catch-all-error-p result) (not result)) nil T))

(defun opor-dimstyle-save-current-native (doc style / pair result)
  ;; A DIMSTYLE arrow ObjectID written only through entmod is not enough:
  ;; AutoCAD's dialog may still show Closed filled. Saving document overrides
  ;; through CopyFrom is the supported path and preserves the UI choice.
  (foreach pair
    (list
      (cons "DIMSCALE" 1.0)
      (cons "DIMASZ" *opor-dimstyle-arrow-size*)
      (cons "DIMCEN" *opor-dimstyle-center-size*)
      (cons "DIMTSZ" 0.0)
      (cons "DIMSAH" 0)
      (cons "DIMBLK" "_OBLIQUE")
      (cons "DIMLDRBLK" ".")
      (cons "DIMEXO" 0.625)
      (cons "DIMDLI" *opor-dimstyle-line-spacing*)
      (cons "DIMEXE" 1.25)
      (cons "DIMDLE" 0.0)
      (cons "DIMARCSYM" 0)
      (cons "DIMJOGANG" (/ pi 2.0))
      (cons "DIMTXSTY" *opor-dimstyle-text-style*)
      (cons "DIMCLRT" 0)
      (cons "DIMTFILL" 0)
      (cons "DIMTXT" *opor-dimstyle-text-height*)
      (cons "DIMTFAC" 1.0)
      (cons "DIMTAD" 1)
      (cons "DIMJUST" 0)
      (cons "DIMTXTDIRECTION" 0)
      (cons "DIMGAP" *opor-dimstyle-text-gap*)
      (cons "DIMTIH" 0)
      (cons "DIMTOH" 0)
      (cons "DIMLUNIT" 2)
      (cons "DIMDEC" 0)
      (cons "DIMDSEP" 44)
      (cons "DIMRND" 0.0)
      (cons "DIMLFAC" 1.0)
      (cons "DIMZIN" 8)
      (cons "DIMAUNIT" 0)
      (cons "DIMADEC" 0)
      (cons "DIMAZIN" 0)
      (cons "DIMPOST" "")
      (cons "DIMAPOST" ""))
    (opor-safe-setvar (car pair) (cdr pair)))
  (setq result (vl-catch-all-apply 'vla-CopyFrom (list style doc)))
  (if (vl-catch-all-error-p result) nil T))

;; ТЗ п.7. Меняем только параметры с трёх присланных вкладок и базовые
;; значения ISO-25; остальные настройки существующего стиля не трогаем.
(defun opor-ensure-dimstyle
  (/ doc dimstyles style style-en text-en arrow-en data result native-ok extended-ok)
  (vl-load-com)
  (setq doc (opor-doc))
  (setq dimstyles (vla-get-DimStyles doc))
  (if (not (tblsearch "STYLE" *opor-dimstyle-text-style*))
    (opor-ensure-drawing-title-style))
  (setq text-en (tblobjname "STYLE" *opor-dimstyle-text-style*))
  (setq style-en (tblobjname "DIMSTYLE" *opor-dimstyle-name*))
  (setq style
    (vl-catch-all-apply 'vla-Item
      (list dimstyles *opor-dimstyle-name*)))
  (if (vl-catch-all-error-p style)
    (progn
      (setq style
        (vl-catch-all-apply 'vla-Add
          (list dimstyles *opor-dimstyle-name*)))
      (if (not (vl-catch-all-error-p style))
        (setq style-en (vlax-vla-object->ename style))))
    (setq style-en (vlax-vla-object->ename style)))
  (setq arrow-en (opor-ensure-oblique-arrow-block))
  (if (and style-en text-en arrow-en)
    (progn
      (setq data (entget style-en))
      ;; Symbols and arrows.
      (setq data (opor-dxf-set data 40 1.0))
      (setq data (opor-dxf-set data 41 *opor-dimstyle-arrow-size*))
      (setq data (opor-dxf-set data 42 0.625))
      (setq data (opor-dxf-set data 43 *opor-dimstyle-line-spacing*))
      (setq data (opor-dxf-set data 44 1.25))
      (setq data (opor-dxf-set data 50 (/ pi 2.0)))
      (setq data (opor-dxf-set data 141 *opor-dimstyle-center-size*))
      (setq data (opor-dxf-set data 142 0.0))
      (setq data (opor-dxf-set data 173 0))
      (setq data (opor-dxf-remove data 341))
      (setq data (opor-dxf-set data 342 arrow-en))
      (setq data (opor-dxf-remove data 343))
      (setq data (opor-dxf-remove data 344))
      ;; Text.
      (setq data (opor-dxf-set data 140 *opor-dimstyle-text-height*))
      (setq data (opor-dxf-set data 146 1.0))
      (setq data (opor-dxf-set data 147 *opor-dimstyle-text-gap*))
      (setq data (opor-dxf-set data 73 0))
      (setq data (opor-dxf-set data 74 0))
      (setq data (opor-dxf-set data 77 1))
      (setq data (opor-dxf-set data 178 0))
      (setq data (opor-dxf-set data 280 0))
      (setq data (opor-dxf-set data 340 text-en))
      ;; Primary and angular units.
      (setq data (opor-dxf-set data 45 0.0))
      (setq data (opor-dxf-set data 78 8))
      (setq data (opor-dxf-set data 79 0))
      (setq data (opor-dxf-set data 144 1.0))
      (setq data (opor-dxf-set data 179 0))
      (setq data (opor-dxf-set data 271 0))
      (setq data (opor-dxf-set data 275 0))
      (setq data (opor-dxf-set data 277 2))
      (setq data (opor-dxf-set data 278 44))
      (setq result (vl-catch-all-apply 'entmod (list data)))
      (if (or (vl-catch-all-error-p result) (not result))
        (progn
          (opor-log "Не удалось создать или обновить размерный стиль ISO-25.")
          nil)
        (progn
          (setq native-ok T)
          ;; Do the native save only when ISO-25 is current. This fixes the
          ;; dialog representation without switching a user's other style.
          (if (= (strcase (getvar "DIMSTYLE"))
                 (strcase *opor-dimstyle-name*))
            (setq native-ok (opor-dimstyle-save-current-native doc style)))
          (setq extended-ok
            (opor-dimstyle-apply-extended-settings style-en))
          (if (not extended-ok)
            (opor-log "ISO-25: не записаны высота излома и выравнивание по ISO."))
          (entupd style-en)
          (if (not native-ok)
            (progn
              (opor-log "ISO-25 обновлён, но штатное сохранение активного стиля не удалось.")
              nil)
            T))))
    (progn
      (opor-log "Размерный стиль ISO-25 не создан: нет стиля isocpeur или стрелки _OBLIQUE.")
      nil)))

(defun opor-check-environment (create-layers / missing-layers missing-known missing-mvp dimstyle-ok msg)
  (vl-load-com)
  (opor-import-support-blocks)
  (opor-import-new-table-blocks)
  (setq dimstyle-ok (opor-ensure-dimstyle))
  (setq missing-layers (opor-missing-layers))
  (if (and create-layers missing-layers)
    (progn
      (opor-ensure-layers)
      (setq missing-layers (opor-missing-layers))))
  (setq missing-known (opor-missing-blocks *opor-known-blocks*))
  (setq missing-mvp (opor-missing-blocks *opor-mvp-required-blocks*))
  (setq msg
    (strcat
      "Проверка OPOR:"
      "\nВерсия: " *opor-version*
      "\nСлои: " (if missing-layers (strcat "не найдены" (opor-join-lines missing-layers)) "OK")
      "\nБлоки MVP: " (if missing-mvp (strcat "не найдены" (opor-join-lines missing-mvp)) "OK")
      "\nРазмерный стиль " *opor-dimstyle-name* ": " (if dimstyle-ok "OK" "ошибка")
      "\nСправочно, отсутствующие будущие блоки: "
      (if missing-known (opor-join-lines missing-known) "\n- нет")))
  (opor-log msg)
  (list
    (cons 'missing-layers missing-layers)
    (cons 'missing-known-blocks missing-known)
    (cons 'missing-mvp-blocks missing-mvp)
    (cons 'dimstyle-ok dimstyle-ok)))

(defun opor-command-check ()
  (opor-check-environment T)
  (princ))

(defun opor-environment-ready-for-mvp-p (/ report missing)
  (setq report (opor-check-environment T))
  (setq missing (cdr (assoc 'missing-mvp-blocks report)))
  (if missing
    (progn
      (opor-alert (strcat "MVP не может стартовать: не найдены блоки." (opor-join-lines missing)))
      nil)
    T))

;; ТЗ П4: крепёж. Лага = по числу опор (ответ заказчика 2026-07-06);
;; плитка = длина лаг / шаг, вверх (временная модель до порта раскладки плитки)
(defun opor-fasteners-count (/ lagf tilef step len qty)
  (setq lagf (opor-session-get 'lag-fastener))
  (opor-session-set 'lag-fastener-count
    (if lagf (opor-session-get 'support-count) 0))
  (setq tilef (opor-session-get 'tile-fastener))
  (setq step (opor-session-get 'tile-fastener-step))
  (cond
    ;; LASTRA стоит непосредственно на каждой опоре, лаговой длины у неё нет.
    ((and tilef (opor-direct-tile-fastener-p tilef))
      (opor-session-set 'tile-fastener-count
        (opor-session-get 'support-count)))
    ((and tilef (numberp step) (> step 0.0))
      (setq len (opor-session-get 'lag-length-mm))
      (if (not (numberp len)) (setq len 0.0))
      (setq qty (fix (/ len step)))
      (if (> len (* qty step)) (setq qty (1+ qty)))
      (opor-session-set 'tile-fastener-count qty))
    (t (opor-session-set 'tile-fastener-count 0)))
  T)

(defun opor-fasteners-log-text (/ lagf tilef text)
  (setq lagf (opor-session-get 'lag-fastener))
  (setq tilef (opor-session-get 'tile-fastener))
  (setq text "")
  (if lagf
    (setq text
      (strcat text ", крепёж лаги (" lagf ")="
        (itoa (opor-session-get 'lag-fastener-count)))))
  (if tilef
    (setq text
      (strcat text ", крепёж плитки (" tilef ")="
        (itoa (opor-session-get 'tile-fastener-count)))))
  (if (> (opor-floor-fastener-count) 0)
    (setq text
      (strcat text ", толщина крепежа="
        (rtos (opor-floor-fastener-thickness) 2 0) " мм")))
  text)

;; ТЗ П5: счётчик лаг (рядов) для лога завершения
(defun opor-lag-count-log-text (/ n)
  (setq n (opor-session-get 'lag-row-count))
  (if (and (opor-floor-uses-lags-p) (numberp n))
    (strcat ", лаг=" (itoa n) " шт")
    ""))

;; VBA starts/fin: несколько контуров считаются в одной сессии, а таблица
;; вставляется один раз после Enter. Длину держим в сырых мм и округляем только
;; общий итог — сумма уже округлённых метров давала бы другой результат.
(defun opor-number-or-zero (value)
  (if (numberp value) value 0))

(defun opor-multi-add-counts (total added / pair old)
  (foreach pair added
    (setq old (assoc (car pair) total))
    (if old
      (setq total
        (subst (cons (car pair) (+ (cdr old) (cdr pair))) old total))
      (setq total (append total (list pair)))))
  total)

(defun opor-multi-init ()
  (opor-session-set 'contour-count 0)
  (opor-session-set 'total-area 0.0)
  (opor-session-set 'total-area-gross 0.0)
  (opor-session-set 'total-perimeter-mm 0.0)
  (opor-session-set 'total-lag-length-mm 0.0)
  (opor-session-set 'total-lag-row-count 0)
  (opor-session-set 'total-support-count 0)
  (opor-session-set 'total-support-counts '())
  (opor-session-set 'total-support-height-errors 0)
  (opor-session-set 'total-support-insert-errors 0)
  (opor-session-set 'total-tile-whole-count 0)
  (opor-session-set 'total-tile-trimmed-count 0)
  (opor-session-set 'total-board-whole-count 0)
  (opor-session-set 'total-board-trimmed-count 0)
  (opor-session-set 'total-lag-fastener-count 0)
  (opor-session-set 'total-tile-fastener-count 0)
  (opor-session-set 'total-level-polyline-count 0)
  (opor-session-set 'total-level-triangle-count 0)
  (opor-session-set 'total-grid-lag-length-mm 0.0)
  (opor-session-set 'total-boundary-outer-lag-length-mm 0.0)
  (opor-session-set 'total-boundary-holes-lag-length-mm 0.0)
  (opor-session-set 'total-boundary-lag-length-mm 0.0)
  (opor-session-set 'total-perp-lines-removed 0)
  (opor-session-set 'total-dbl-lag-created 0)
  (opor-session-set 'total-dbl-lag-deduped 0)
  (opor-session-set 'total-dbl-lag-joint-count 0)
  (opor-session-set 'total-dbl-lag-pair-segments 0)
  (opor-session-set 'total-dbl-lag-stagger-support-count 0)
  (opor-session-set 'total-support-vertex-count 0)
  (opor-session-set 'total-support-raw-border-count 0)
  (opor-session-set 'total-support-border-count 0)
  (opor-session-set 'total-support-raw-node-count 0)
  (opor-session-set 'total-support-after-self-node-count 0)
  (opor-session-set 'total-support-after-vertex-node-count 0)
  (opor-session-set 'total-support-node-count 0)
  (opor-session-set 'total-support-overlap-deduped 0)
  (opor-session-set 'total-support-excluded-curve-vertex-count 0)
  (opor-session-set 'total-level-mark-count 0)
  (opor-session-set 'total-level-vertex-count 0)
  (opor-session-set 'total-level-missing-mark-count 0)
  (opor-session-set 'total-level-missing-contour-count 0)
  T)

(defun opor-multi-reset-current (/ key)
  (foreach key
    '(lag-length-mm lag-length-m lag-row-count support-count
      support-height-errors support-insert-errors tile-whole-count tile-trimmed-count
      board-whole-count board-trimmed-count
      lag-fastener-count tile-fastener-count level-polyline-count
      level-triangle-count grid-lag-length-mm boundary-outer-lag-length-mm
      boundary-holes-lag-length-mm boundary-lag-length-mm perp-lines-removed
      dbl-lag-created dbl-lag-deduped dbl-lag-joint-count
      dbl-lag-pair-segments dbl-lag-stagger-support-count support-vertex-count
      support-raw-border-count support-border-count support-raw-node-count
      support-after-self-node-count support-after-vertex-node-count
      support-node-count support-overlap-deduped support-excluded-curve-vertex-count
      level-mark-count level-vertex-count
      level-missing-mark-count level-missing-contour-count)
    (opor-session-set key 0))
  (opor-session-set 'support-counts '())
  (opor-session-set 'support-blocks '())
  (opor-session-set 'var-aborted nil)
  T)

(defun opor-multi-accumulate (/ n pair)
  (setq n (1+ (opor-number-or-zero (opor-session-get 'contour-count))))
  (opor-session-set 'contour-count n)
  (opor-session-set 'total-area
    (+ (opor-number-or-zero (opor-session-get 'total-area))
       (opor-number-or-zero (opor-session-get 'area))))
  (opor-session-set 'total-area-gross
    (+ (opor-number-or-zero (opor-session-get 'total-area-gross))
       (opor-number-or-zero (opor-session-get 'area-gross))))
  (opor-session-set 'total-perimeter-mm
    (+ (opor-number-or-zero (opor-session-get 'total-perimeter-mm))
       (opor-number-or-zero (opor-session-get 'perimeter-mm))))
  (opor-session-set 'total-lag-length-mm
    (+ (opor-number-or-zero (opor-session-get 'total-lag-length-mm))
       (opor-number-or-zero (opor-session-get 'lag-length-mm))))
  (opor-session-set 'total-lag-row-count
    (+ (opor-number-or-zero (opor-session-get 'total-lag-row-count))
       (opor-number-or-zero (opor-session-get 'lag-row-count))))
  (opor-session-set 'total-support-count
    (+ (opor-number-or-zero (opor-session-get 'total-support-count))
       (opor-number-or-zero (opor-session-get 'support-count))))
  (opor-session-set 'total-support-counts
    (opor-multi-add-counts
      (opor-session-get 'total-support-counts)
      (opor-session-get 'support-counts)))
  (foreach pair
    '((total-support-height-errors . support-height-errors)
      (total-support-insert-errors . support-insert-errors)
      (total-tile-whole-count . tile-whole-count)
      (total-tile-trimmed-count . tile-trimmed-count)
      (total-board-whole-count . board-whole-count)
      (total-board-trimmed-count . board-trimmed-count)
      (total-lag-fastener-count . lag-fastener-count)
      (total-tile-fastener-count . tile-fastener-count)
      (total-level-polyline-count . level-polyline-count)
      (total-level-triangle-count . level-triangle-count)
      (total-grid-lag-length-mm . grid-lag-length-mm)
      (total-boundary-outer-lag-length-mm . boundary-outer-lag-length-mm)
      (total-boundary-holes-lag-length-mm . boundary-holes-lag-length-mm)
      (total-boundary-lag-length-mm . boundary-lag-length-mm)
      (total-perp-lines-removed . perp-lines-removed)
      (total-dbl-lag-created . dbl-lag-created)
      (total-dbl-lag-deduped . dbl-lag-deduped)
      (total-dbl-lag-joint-count . dbl-lag-joint-count)
      (total-dbl-lag-pair-segments . dbl-lag-pair-segments)
      (total-dbl-lag-stagger-support-count . dbl-lag-stagger-support-count)
      (total-support-vertex-count . support-vertex-count)
      (total-support-raw-border-count . support-raw-border-count)
      (total-support-border-count . support-border-count)
      (total-support-raw-node-count . support-raw-node-count)
      (total-support-after-self-node-count . support-after-self-node-count)
      (total-support-after-vertex-node-count . support-after-vertex-node-count)
      (total-support-node-count . support-node-count)
      (total-support-overlap-deduped . support-overlap-deduped)
      (total-support-excluded-curve-vertex-count . support-excluded-curve-vertex-count)
      (total-level-mark-count . level-mark-count)
      (total-level-vertex-count . level-vertex-count)
      (total-level-missing-mark-count . level-missing-mark-count)
      (total-level-missing-contour-count . level-missing-contour-count))
    (opor-session-set (car pair)
      (+ (opor-number-or-zero (opor-session-get (car pair)))
         (opor-number-or-zero (opor-session-get (cdr pair))))))
  n)

(defun opor-multi-apply-totals ()
  (opor-session-set 'area (opor-session-get 'total-area))
  (opor-session-set 'area-gross (opor-session-get 'total-area-gross))
  (opor-session-set 'perimeter-mm (opor-session-get 'total-perimeter-mm))
  (opor-session-set 'lag-length-mm (opor-session-get 'total-lag-length-mm))
  (opor-session-set 'lag-length-m
    (opor-round-half-even
      (/ (opor-number-or-zero (opor-session-get 'total-lag-length-mm)) 1000.0)))
  (opor-session-set 'lag-row-count (opor-session-get 'total-lag-row-count))
  (opor-session-set 'support-count (opor-session-get 'total-support-count))
  (opor-session-set 'support-counts (opor-session-get 'total-support-counts))
  (opor-session-set 'support-height-errors (opor-session-get 'total-support-height-errors))
  (opor-session-set 'support-insert-errors (opor-session-get 'total-support-insert-errors))
  (opor-session-set 'tile-whole-count (opor-session-get 'total-tile-whole-count))
  (opor-session-set 'tile-trimmed-count (opor-session-get 'total-tile-trimmed-count))
  (opor-session-set 'board-whole-count (opor-session-get 'total-board-whole-count))
  (opor-session-set 'board-trimmed-count (opor-session-get 'total-board-trimmed-count))
  (opor-session-set 'lag-fastener-count (opor-session-get 'total-lag-fastener-count))
  (opor-session-set 'tile-fastener-count (opor-session-get 'total-tile-fastener-count))
  (opor-session-set 'level-polyline-count (opor-session-get 'total-level-polyline-count))
  (opor-session-set 'level-triangle-count (opor-session-get 'total-level-triangle-count))
  (opor-session-set 'grid-lag-length-mm (opor-session-get 'total-grid-lag-length-mm))
  (opor-session-set 'boundary-outer-lag-length-mm (opor-session-get 'total-boundary-outer-lag-length-mm))
  (opor-session-set 'boundary-holes-lag-length-mm (opor-session-get 'total-boundary-holes-lag-length-mm))
  (opor-session-set 'boundary-lag-length-mm (opor-session-get 'total-boundary-lag-length-mm))
  (opor-session-set 'perp-lines-removed (opor-session-get 'total-perp-lines-removed))
  (opor-session-set 'dbl-lag-created (opor-session-get 'total-dbl-lag-created))
  (opor-session-set 'dbl-lag-deduped (opor-session-get 'total-dbl-lag-deduped))
  (opor-session-set 'dbl-lag-joint-count (opor-session-get 'total-dbl-lag-joint-count))
  (opor-session-set 'dbl-lag-pair-segments (opor-session-get 'total-dbl-lag-pair-segments))
  (opor-session-set 'dbl-lag-stagger-support-count (opor-session-get 'total-dbl-lag-stagger-support-count))
  (opor-session-set 'support-vertex-count (opor-session-get 'total-support-vertex-count))
  (opor-session-set 'support-raw-border-count (opor-session-get 'total-support-raw-border-count))
  (opor-session-set 'support-border-count (opor-session-get 'total-support-border-count))
  (opor-session-set 'support-raw-node-count (opor-session-get 'total-support-raw-node-count))
  (opor-session-set 'support-after-self-node-count (opor-session-get 'total-support-after-self-node-count))
  (opor-session-set 'support-after-vertex-node-count (opor-session-get 'total-support-after-vertex-node-count))
  (opor-session-set 'support-node-count (opor-session-get 'total-support-node-count))
  (opor-session-set 'support-overlap-deduped
    (opor-session-get 'total-support-overlap-deduped))
  (opor-session-set 'support-excluded-curve-vertex-count
    (opor-session-get 'total-support-excluded-curve-vertex-count))
  (opor-session-set 'level-mark-count (opor-session-get 'total-level-mark-count))
  (opor-session-set 'level-vertex-count (opor-session-get 'total-level-vertex-count))
  (opor-session-set 'level-missing-mark-count (opor-session-get 'total-level-missing-mark-count))
  (opor-session-set 'level-missing-contour-count (opor-session-get 'total-level-missing-contour-count))
  (if (= (opor-session-get 'mode) "var-height")
    (progn
      (opor-session-set 'floor-height (opor-session-get 'multi-floor-height-base))
      (opor-session-set 'zfloor 0.0)))
  T)

(defun opor-multi-log-contour (mode / n)
  (setq n (opor-session-get 'contour-count))
  (opor-log
    (strcat
      "Контур " (itoa n) " рассчитан: опор="
      (itoa (opor-number-or-zero (opor-session-get 'support-count)))
      ", длина лаг="
      (rtos (/ (opor-number-or-zero (opor-session-get 'lag-length-mm)) 1000.0) 2 3)
      " м"
      (if (= mode "var-height")
        (strcat ", ошибок высот="
          (itoa (opor-number-or-zero (opor-session-get 'support-height-errors))))
        "")
      (if (> (opor-number-or-zero (opor-session-get 'support-insert-errors)) 0)
        (strcat ", ошибок вставки опор="
          (itoa (opor-number-or-zero (opor-session-get 'support-insert-errors))))
        "")
      (if (> (opor-number-or-zero (opor-session-get 'support-overlap-deduped)) 0)
        (strcat ", удалено перекрывающихся="
          (itoa (opor-number-or-zero (opor-session-get 'support-overlap-deduped))))
        "")
      ".")))

(defun opor-run-const-height (/ boundary holes grid supports table done first-p insert-errors)
  (if (not (opor-environment-ready-for-mvp-p))
    nil
    (progn
      (opor-view-save)
      (opor-multi-init)
      (setq done nil)
      (while (not done)
        (setq first-p (= (opor-session-get 'contour-count) 0))
        (if (not first-p) (opor-view-show-saved))
        (opor-layers-hide)
        (setq boundary (opor-select-outer-boundary))
        (if (not boundary)
          (progn (opor-layers-restore) (setq done T))
          (progn
            (opor-zoom-to-boundary boundary)
            (opor-session-set 'outer-boundary boundary)
            (setq holes (opor-detect-holes boundary))
            (if (not (opor-hole-regions-valid-p holes))
              (progn
                (opor-log "Const остановлен: некорректная геометрия проёмов.")
                (opor-layers-restore)
                (setq done T))
              (progn
                (opor-session-set 'holes holes)
                (opor-session-set 'area-gross (vla-get-Area boundary))
                (opor-session-set 'area (opor-net-area boundary holes))
                (opor-session-set 'perimeter-mm (opor-curve-length boundary))
                (opor-layers-restore)
                (opor-multi-reset-current)
                (if (if first-p (opor-ui-read-params) (opor-ui-read-next-contour-points))
                  (progn
                    (setq grid (opor-grid-build *opor-session*))
                    (if grid
                      (progn
                        (if (= (opor-session-get 'tile-mode) "p")
                          (opor-tiles-run *opor-session*)
                          (opor-boards-count-run *opor-session*))
                        (setq supports (opor-supports-place *opor-session*))
                        (opor-fasteners-count)
                        (opor-multi-accumulate)
                        (opor-multi-log-contour "const-height"))
                      (setq done T)))
                  (setq done T)))))))
      (opor-layers-restore)
      (if (> (opor-session-get 'contour-count) 0)
        (progn
          (opor-multi-apply-totals)
          (setq insert-errors (opor-session-get 'support-insert-errors))
          (setq table (opor-insert-total-table *opor-session*))
          (if table
            (progn
              (if (> insert-errors 0)
                (opor-alert
                  (strcat
                    "Не удалось вставить опоры: " (itoa insert-errors) ".\n"
                    "Итоговая таблица содержит только реально созданные опоры.")))
              (opor-log
                (strcat
                  "MVP завершён: опор="
                  (itoa (opor-session-get 'support-count))
                  ", длина лаг="
                  (itoa (opor-session-get 'lag-length-m))
                  " м"
                  (opor-lag-count-log-text)
                  (if (> (opor-session-get 'contour-count) 1)
                    (strcat ", контуров=" (itoa (opor-session-get 'contour-count)))
                    "")
                  (opor-tiles-log-text)
                  (opor-dbl-lag-log-text)
                  (opor-fasteners-log-text)
                  ", ошибок вставки опор=" (itoa insert-errors)
                  "."))
              (opor-view-restore)
              (= insert-errors 0))
            (progn
              (opor-alert
                "Расчёт выполнен, но итоговая таблица не вставлена. Команда завершена с ошибкой.")
              (opor-log "MVP не завершён: итоговая таблица не вставлена.")
              (opor-view-restore)
              nil)))
        (progn (opor-view-restore) nil)))))

(defun opor-run-var-height (/ boundary holes grid supports table errors insert-errors done first-p maxmark)
  (if (not (opor-environment-ready-for-mvp-p))
    nil
    (progn
      (opor-view-save)
      (opor-multi-init)
      (opor-session-set 'mode "var-height")
      (setq done nil)
      (while (not done)
        (setq first-p (= (opor-session-get 'contour-count) 0))
        (if (not first-p) (opor-view-show-saved))
        (opor-layers-hide)
        (setq boundary (opor-pick-boundary-with-levels-hidden))
        (if (not boundary)
          (progn (opor-layers-restore) (setq done T))
          (progn
            (opor-zoom-to-boundary boundary)
            (opor-session-set 'outer-boundary boundary)
            (setq holes (opor-detect-holes boundary))
            (if (not (opor-hole-regions-valid-p holes))
              (progn
                (opor-log "Var остановлен: некорректная геометрия проёмов.")
                (opor-layers-restore)
                (setq done T))
              (progn
                (opor-session-set 'holes holes)
                (opor-session-set 'area-gross (vla-get-Area boundary))
                (opor-session-set 'area (opor-net-area boundary holes))
                (opor-session-set 'perimeter-mm (opor-curve-length boundary))
                (opor-layers-restore)
                (opor-multi-reset-current)
                (if (not (opor-level-scan *opor-session*))
                  (progn
                    (opor-log "Var остановлен: проверка отметок не пройдена.")
                    (setq done T))
                  (progn
                    (if first-p
                      (if (opor-ui-read-var-params)
                        (opor-session-set 'multi-floor-height-base
                          (opor-session-get 'floor-height))
                        (setq done T))
                      (progn
                        (opor-session-set 'floor-height
                          (opor-session-get 'multi-floor-height-base))
                        (opor-session-set 'zfloor 0.0)
                        (setq maxmark (opor-level-max-mark *opor-session*))
                        (if (and maxmark
                                 (>= maxmark
                                   (opor-round-half-even
                                     (opor-session-get 'multi-floor-height-base))))
                          (progn
                            (opor-alert
                              (strcat
                                "Максимальная отметка=" (opor-height-text maxmark)
                                "\nУровень чистого пола="
                                (opor-height-text (opor-session-get 'multi-floor-height-base))
                                "\nУровень чистого пола должен быть выше максимальной отметки."))
                            (setq done T))
                          (if (not (opor-ui-read-next-contour-points))
                            (setq done T)))))
                    (if (not done)
                      (progn
                        (opor-level-apply-zfloor *opor-session*)
                        (opor-level-triangulate *opor-session*)
                        (if (opor-session-get 'show-triangles)
                          (opor-level-draw-triangles))
                        (setq grid (opor-grid-build *opor-session*))
                        (if (not grid)
                          (setq done T)
                          (progn
                            (if (= (opor-session-get 'tile-mode) "p")
                              (opor-tiles-run *opor-session*)
                              (opor-boards-count-run *opor-session*))
                            (setq supports (opor-supports-place-variable *opor-session*))
                            (if (opor-session-get 'var-aborted)
                              (progn
                                (opor-log "Var прерван: точки вне границ областей высот; текущий контур не включён в таблицу.")
                                (setq done T))
                              (progn
                                (opor-fasteners-count)
                                (opor-multi-accumulate)
                                (opor-multi-log-contour "var-height"))))))))))))))
      (opor-layers-restore)
      (if (> (opor-session-get 'contour-count) 0)
        (progn
          (opor-multi-apply-totals)
          (setq errors (opor-session-get 'support-height-errors))
          (setq insert-errors (opor-session-get 'support-insert-errors))
          (setq table (opor-insert-total-table *opor-session*))
          (if table
            (progn
              (if (> errors 0)
                (opor-alert
                  (strcat
                    "Не определены высоты для количества опор: "
                    (itoa errors)
                    "\nОпоры окрашены в оранжевый цвет.")))
              (if (> insert-errors 0)
                (opor-alert
                  (strcat
                    "Не удалось вставить опоры: " (itoa insert-errors) ".\n"
                    "Итоговая таблица содержит только реально созданные опоры.")))
              (opor-log
                (strcat
                  "Var завершён: опор="
                  (itoa (opor-session-get 'support-count))
                  ", длина лаг="
                  (itoa (opor-session-get 'lag-length-m))
                  " м"
                  (opor-lag-count-log-text)
                  (if (> (opor-session-get 'contour-count) 1)
                    (strcat ", контуров=" (itoa (opor-session-get 'contour-count)))
                    "")
                  ", ошибок высот="
                  (itoa errors)
                  ", ошибок вставки опор="
                  (itoa insert-errors)
                  ", областей="
                  (itoa (opor-session-get 'level-polyline-count))
                  ", треугольников="
                  (itoa (opor-session-get 'level-triangle-count))
                  (opor-tiles-log-text)
                  (opor-dbl-lag-log-text)
                  (opor-fasteners-log-text)
                  "."))
              (opor-view-restore)
              (= insert-errors 0))
            (progn
              (opor-alert
                "Расчёт Var выполнен, но итоговая таблица не вставлена. Команда завершена с ошибкой.")
              (opor-log "Var не завершён: итоговая таблица не вставлена.")
              (opor-view-restore)
              nil)))
        (progn (opor-view-restore) nil)))))

(defun opor-main (/ mode)
  (setq mode (opor-ui-select-mode))
  ;; Clean должен видеть предыдущую сессию. Раньше новый пустой session создавался
  ;; ещё до выбора режима, поэтому OPOR -> Clean всегда сообщал 0 объектов.
  (if (/= mode "clean") (opor-init-session))
  (cond
    ((= mode "const-height") (opor-run-const-height))
    ((= mode "var-height") (opor-run-var-height))
    ((= mode "slope") (opor-slope-run))
    ((= mode "slopewr") (opor-slope-write-run))
    ((= mode "slope-levels") (opor-slope-level-run))
    ((= mode "height-check") (opor-height-check-run))
    ((= mode "write-level") (opor-write-level-run))
    ((= mode "auto-level") (opor-auto-level-run))
    ((= mode "ring") (opor-ring-run))
    ((= mode "tin") (opor-tin-run))
    ((= mode "geo-levels") (opor-geo-run))
    ((= mode "check") (opor-command-check))
    ((= mode "clean") (opor-command-clean))
    (t (opor-log "Команда отменена.")))
  ;; UX: слои возвращаются на ЛЮБОМ пути выхода (отмена контура/формы и т.п.)
  (opor-layers-restore)
  (princ))

(defun opor-debug-session ()
  (princ "\n--- OPOR SESSION ---")
  (foreach pair *opor-session*
    (princ (strcat "\n" (vl-princ-to-string (car pair)) " = " (vl-princ-to-string (cdr pair)))))
  (princ))

(princ)
