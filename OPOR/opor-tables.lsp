;;; OPOR support source tables and total block tables

;; variant от GetCellValue надо развернуть до значения
(defun opor-variant-string (value)
  (if (= (type value) 'variant)
    (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray)
    ""
    (opor-string value)))

;; срезать MText-коды: "{\C221;221" -> "221"
(defun opor-strip-mtext (text / pos semi)
  (setq text (opor-string text))
  (setq text (opor-string-replace-all "{" "" text))
  (setq text (opor-string-replace-all "}" "" text))
  (while (setq pos (vl-string-search "\\" text))
    (setq semi (vl-string-search ";" text pos))
    (setq text
      (strcat
        (substr text 1 pos)
        (if semi (substr text (+ semi 2)) ""))))
  text)

(defun opor-table-cell-text (table row col / value)
  (setq value (vl-catch-all-apply 'vla-GetText (list table row col)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-GetCellValue (list table row col)))
      (if (vl-catch-all-error-p value) "" (opor-variant-string value)))
    (opor-strip-mtext (opor-string value))))

(defun opor-table-cell-value-text (table row col / value)
  (setq value (vl-catch-all-apply 'vla-GetCellValue (list table row col)))
  (if (vl-catch-all-error-p value)
    (opor-table-cell-text table row col)
    (opor-strip-mtext (opor-variant-string value))))

(defun opor-find-support-table (/ found)
  (setq found nil)
  (vlax-for obj (opor-ms)
    (if (and (not found)
             (= (opor-obj-name obj) "AcDbTable")
             (= (opor-table-cell-text obj 0 0) "Опоры"))
      (setq found obj)))
  found)

(defun opor-table-row-count (table / value)
  (setq value (vl-catch-all-apply 'vla-get-Rows (list table)))
  (if (vl-catch-all-error-p value) 0 value))

(defun opor-support-row-for-line-p (name line)
  (cond
    ((= line "3D")
      (and (wcmatch (strcase name) "*3D*")
           (not (wcmatch (strcase name) "*3D*0-35*"))))
    ((= line "PRO") (wcmatch (strcase name) "*PRO*"))
    (t (and (not (wcmatch (strcase name) "*3D*"))
            (not (wcmatch (strcase name) "*PRO*"))))))

(defun opor-parse-int (text default / value)
  (setq text (vl-string-trim " " (opor-strip-mtext (opor-string text))))
  (if (= text "")
    default
    (progn
      (setq value (vl-catch-all-apply 'read (list text)))
      (if (or (vl-catch-all-error-p value) (not (numberp value)))
        default
        (fix value)))))

(defun opor-string-replace-all (from to text / pos)
  (while (setq pos (vl-string-search from text))
    (setq text (vl-string-subst to from text pos)))
  text)

(defun opor-clean-number-text (text)
  (setq text (vl-string-trim " " (opor-string text)))
  (setq text (opor-string-replace-all "+" "" text))
  (setq text (opor-string-replace-all " " "" text))
  (vl-string-translate "," "." text))

(defun opor-parse-real (text default / value)
  (setq text (opor-clean-number-text text))
  (if (= text "")
    default
    (progn
      (setq value (vl-catch-all-apply 'read (list text)))
      (if (or (vl-catch-all-error-p value) (not (numberp value)))
        default
        value))))

(defun opor-support-range-limits (range / clean parts low high)
  (setq clean (opor-clean-number-text range))
  (setq clean (opor-string-replace-all "*" "" clean))
  (cond
    ((= clean "") nil)
    ((vl-string-search "<" clean)
      (setq high (opor-parse-real (opor-string-replace-all "<" "" clean) nil))
      (if high (list 0.0 high) nil))
    ((vl-string-search "-" clean)
      (setq parts (read (strcat "(\"" (opor-string-replace-all "-" "\" \"" clean) "\")")))
      (setq low (opor-parse-real (car parts) nil))
      (setq high (opor-parse-real (cadr parts) nil))
      (if (and low high) (list low high) nil))
    (t nil)))

(defun opor-built-in-support-row (index name range color low high)
  (list
    (cons 'index index)
    (cons 'name name)
    (cons 'range range)
    (cons 'color color)
    (cons 'min low)
    (cons 'max high)))

;; ТЗ П2: старый ACI 82 больше не используется для опор 315-530.
;; Нормализация действует и для встроенного каталога, и для старой таблицы
;; «Опоры» в пользовательском DWG, если она всё ещё содержит 82.
(defun opor-support-approved-color (color)
  (if (= color 82) 6 color))

;; ТЗ Дмитрия: актуальные линейки 3D/PRO не должны зависеть от служебной
;; AcDbTable "Опоры" в каждом новом DWG. Диапазон range — подпись изделия;
;; min/max — фактические непересекающиеся пределы подбора из каталога VBA.
(defun opor-built-in-supports (line)
  (cond
    ((= line "3D")
      (list
        (opor-built-in-support-row 1  "3D 35-50"   "35-50"   92  35.0  50.0)
        (opor-built-in-support-row 2  "3D 50-80"   "50-80"   131 50.0  80.0)
        (opor-built-in-support-row 3  "3D 80-140"  "80-140"  30  80.0  140.0)
        (opor-built-in-support-row 4  "3D 95-155"  "95-155"  160 140.0 155.0)
        (opor-built-in-support-row 5  "3D 145-240" "145-240" 60  155.0 240.0)
        (opor-built-in-support-row 6  "3D 160-270" "160-270" 134 240.0 270.0)
        (opor-built-in-support-row 7  "3D 205-340" "205-340" 42  270.0 340.0)
        (opor-built-in-support-row 8  "3D 235-400" "235-400" 26  340.0 400.0)
        (opor-built-in-support-row 9  "3D 315-530" "315-530" 6   400.0 530.0)
        (opor-built-in-support-row 10 "3D 390-660" "390-660" 163 530.0 660.0)))
    ((= line "PRO")
      (list
        (opor-built-in-support-row 1  "Low 12"      "12"      221 0.0   17.0)
        (opor-built-in-support-row 2  "Low 20"      "20"      2   17.0  25.0)
        (opor-built-in-support-row 3  "PRO 25-35"   "25-35"   1   25.0  35.0)
        (opor-built-in-support-row 4  "PRO 35-50"   "35-50"   92  35.0  50.0)
        (opor-built-in-support-row 5  "PRO 50-80"   "50-80"   131 50.0  80.0)
        (opor-built-in-support-row 6  "PRO 80-140"  "80-140"  30  80.0  140.0)
        (opor-built-in-support-row 7  "PRO 95-155"  "95-155"  160 140.0 155.0)
        (opor-built-in-support-row 8  "PRO 145-240" "145-240" 60  155.0 240.0)
        (opor-built-in-support-row 9  "PRO 160-270" "160-270" 134 240.0 270.0)
        (opor-built-in-support-row 10 "PRO 205-340" "205-340" 42  270.0 340.0)
        (opor-built-in-support-row 11 "PRO 235-400" "235-400" 26  340.0 400.0)
        (opor-built-in-support-row 12 "PRO 315-530" "315-530" 6   400.0 530.0)
        (opor-built-in-support-row 13 "PRO 390-660" "390-660" 163 530.0 660.0)))
    (t nil)))

(defun opor-read-supports (line / table rows row name range color result index limits)
  (setq table (opor-find-support-table))
  (setq result '())
  (setq index 1)
  (if table
    (progn
      (setq rows (opor-table-row-count table))
      (setq row 2)
      (while (< row rows)
        (setq name (vl-string-trim " " (opor-table-cell-text table row 0)))
        (setq range (vl-string-trim " " (opor-table-cell-text table row 1)))
        (setq color (vl-string-trim " " (opor-table-cell-value-text table row 2)))
        (if (and (/= name "") (opor-support-row-for-line-p name line))
          (progn
            (setq limits (opor-support-range-limits range))
            (setq result
              (cons
                (list
                  (cons 'index index)
                  (cons 'name name)
                  (cons 'range range)
                  (cons 'color (opor-support-approved-color (opor-parse-int color 256)))
                  (cons 'min (if limits (car limits) nil))
                  (cons 'max (if limits (cadr limits) nil)))
                result))
            (setq index (1+ index))))
        (setq row (1+ row)))))
  (setq result (reverse result))
  (if result result (opor-built-in-supports line)))

(defun opor-table-block-name (line)
  (cdr (assoc line *opor-total-table-by-line*)))

(defun opor-new-table-blocks-ready-p ()
  (and
    (opor-block-exists-p *opor-new-3d-support-table-block*)
    (opor-block-exists-p *opor-new-pro-support-table-block*)
    (opor-block-exists-p *opor-new-tile-params-block*)
    (opor-block-exists-p *opor-new-board-params-block*)))

(defun opor-table-block-library-path (/ path)
  (setq path
    (if (and (boundp '*opor-root*) *opor-root*)
      (strcat (vl-string-right-trim "\\/" *opor-root*) "\\" *opor-table-block-library*)
      *opor-table-block-library*))
  (findfile path))

;; Импорт общей DWG-библиотеки через командную строку нужен как fallback,
;; когда ActiveX/ActiveDocument недоступен (например, в Core Console).
(defun opor-import-block-library-command
  (path / before oldreq olddia result ref data)
  (setq before (entlast)
        oldreq (getvar "ATTREQ")
        olddia (getvar "ATTDIA"))
  (setvar "ATTREQ" 0)
  (setvar "ATTDIA" 0)
  (setq result
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "_.-INSERT" path "_non" '(0.0 0.0 0.0)
        1.0 1.0 0.0)))
  (setq ref (entlast)
        data (if ref (entget ref) nil))
  (if (and ref (/= ref before) (= (cdr (assoc 0 data)) "INSERT"))
    (entdel ref))
  (setvar "ATTREQ" oldreq)
  (setvar "ATTDIA" olddia)
  (if (vl-catch-all-error-p result) nil T))

;; Блок отметки используется не только геологией: он нужен команде +0.000
;; и обратному расчёту отметок. Поэтому его импорт относится к базовому модулю.
(defun opor-import-level-block (/ path imported)
  (if (opor-block-exists-p *opor-level-block-name*)
    T
    (progn
      (setq path (opor-table-block-library-path))
      (if path
        (progn
          (setq imported
            (vl-catch-all-apply
              '(lambda ()
                 (vla-InsertBlock
                   (opor-ms) (vlax-3d-point '(0.0 0.0 0.0)) path
                   1.0 1.0 1.0 0.0))
              nil))
          (if (not (vl-catch-all-error-p imported))
            (vl-catch-all-apply 'vla-Delete (list imported)))
          (if (not (opor-block-exists-p *opor-level-block-name*))
            (opor-import-block-library-command path))))
      (if (opor-block-exists-p *opor-level-block-name*)
        (progn
          (opor-log "Блок otmetka_oporvb загружен из библиотеки.")
          T)
        nil))))

(defun opor-import-new-table-blocks (/ path imported)
  (if (opor-new-table-blocks-ready-p)
    T
    (progn
      (setq path (opor-table-block-library-path))
      (if path
        (progn
          (setq imported
            (vl-catch-all-apply
              'vla-InsertBlock
              (list (opor-ms) (vlax-3d-point '(0.0 0.0 0.0)) path
                    1.0 1.0 1.0 0.0)))
          (if (not (vl-catch-all-error-p imported))
            (vl-catch-all-apply 'vla-Delete (list imported))))
        (opor-log (strcat "Не найдена библиотека новых таблиц: " *opor-table-block-library*)))
      (if (opor-new-table-blocks-ready-p)
        (progn (opor-log "Блоки новых таблиц 3D/PRO загружены.") T)
        (progn (opor-log "Новые таблицы 3D/PRO не загружены; будет использована старая таблица.") nil)))))

;; Клиентский визуал table_slope хранится в той же переносимой DWG-библиотеке,
;; но не участвует в готовности основных таблиц 3D/PRO. Поэтому старые пакеты
;; без этого определения по-прежнему могут считать A/B, а Slope при необходимости
;; отдельно пытается дозагрузить именно свою таблицу.
(defun opor-import-slope-table-block (/ path imported)
  (if (opor-block-exists-p "table_slope")
    T
    (progn
      (setq path (opor-table-block-library-path))
      (if path
        (progn
          (setq imported
            (vl-catch-all-apply
              'vla-InsertBlock
              (list (opor-ms) (vlax-3d-point '(0.0 0.0 0.0)) path
                    1.0 1.0 1.0 0.0)))
          (if (not (vl-catch-all-error-p imported))
            (vl-catch-all-apply 'vla-Delete (list imported))))
        (opor-log
          (strcat "Не найдена библиотека таблицы slope: "
            *opor-table-block-library*)))
      (if (opor-block-exists-p "table_slope")
        (progn
          (opor-log "Клиентский блок table_slope загружен из библиотеки.")
          T)
        (progn
          (opor-log "Клиентский блок table_slope не загружен.")
          nil)))))

(defun opor-set-attribute-values (block values / raw atts tag pair)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (opor-variant-list raw))
      (foreach att atts
        (setq tag (strcase (vla-get-TagString att)))
        (setq pair (assoc tag values))
        (if pair
          (vla-put-TextString att (cdr pair)))))))

(defun opor-block-reference-attribute-count (block / raw)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (vl-catch-all-error-p raw)
    0
    (length (opor-variant-list raw))))

;; Старые итоговые блоки имеют числовые теги диапазонов. Новое требование:
;; пустая ячейка вместо нуля, поэтому сначала явно очищаем все возможные теги.
(defun opor-empty-number-tag-values (/ index values)
  (setq index 1 values '())
  (while (<= index 13)
    (setq values (append values (list (cons (itoa index) ""))))
    (setq index (1+ index)))
  values)

(defun opor-number-tag-values (support-index qop / values)
  (setq values (opor-empty-number-tag-values))
  (if (and (numberp support-index) (> support-index 0))
    (setq values
      (opor-table-value-put
        (itoa support-index)
        (opor-table-nonzero-int-text qop)
        values)))
  values)

(defun opor-number-tag-values-from-counts (counts / values)
  (setq values (opor-empty-number-tag-values))
  (foreach pair counts
    (setq values
      (opor-table-value-put
        (itoa (car pair))
        (opor-table-nonzero-int-text (cdr pair))
        values)))
  values)

(defun opor-counts-total (counts / total)
  (setq total 0)
  (foreach pair counts
    (setq total (+ total (cdr pair))))
  total)

;; s_dyn_blk: Visibility1 = "Доска"/"Плитка"
(defun opor-set-dynamic-visibility (block vis / raw props name)
  (setq raw (vl-catch-all-apply 'vla-GetDynamicBlockProperties (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq props (opor-variant-list raw))
      (foreach prop props
        (setq name (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
        (if (and (not (vl-catch-all-error-p name)) (= name "Visibility1"))
          (vl-catch-all-apply
            'vla-put-Value
            (list prop (vlax-make-variant vis vlax-vbString))))))))

(defun opor-support-max-range (supports / value maxv)
  (setq maxv nil)
  (foreach item supports
    (setq value (cdr (assoc 'max item)))
    (if (and value (or (not maxv) (> value maxv)))
      (setq maxv value)))
  maxv)

(defun opor-support-for-height (height supports / found maxv low high)
  (setq found nil)
  (setq maxv (opor-support-max-range supports))
  (foreach item supports
    (if (not found)
      (progn
        (setq low (cdr (assoc 'min item)))
        (setq high (cdr (assoc 'max item)))
        (if (and low high
                 (>= height low)
                 (or (< height high)
                     (and maxv (= high maxv) (= height high))))
          (setq found item)))))
  found)

(defun opor-inc-index-count (index counts / pair)
  (setq pair (assoc index counts))
  (if pair
    (subst (cons index (1+ (cdr pair))) pair counts)
    (cons (cons index 1) counts)))

(defun opor-first-attribute-text (block / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (opor-variant-list raw))
      (if atts (vla-get-TextString (car atts)) nil))
    nil))

(defun opor-effective-block-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun opor-color-count-key (index color)
  (strcat (itoa index) ":" (itoa color)))

(defun opor-color-count-add (index color counts / key pair)
  (setq key (opor-color-count-key index color))
  (setq pair (assoc key counts))
  (if pair
    (subst (list key index color (1+ (cadddr pair))) pair counts)
    (cons (list key index color 1) counts)))

(defun opor-best-color-for-index (index counts / best)
  (setq best nil)
  (foreach item counts
    (if (and (= (cadr item) index)
             (or (not best) (> (cadddr item) (cadddr best))))
      (setq best item)))
  (if best (caddr best) nil))

(defun opor-infer-support-colors (line supports / block-name counts obj name attr height support index color result inferred)
  (setq block-name (opor-support-block-name line))
  (setq counts '())
  (vlax-for obj (opor-ms)
    (if (and (= (opor-obj-name obj) "AcDbBlockReference")
             (not (opor-object-has-opor-xdata-p obj))
             (= (opor-effective-block-name obj) block-name))
      (progn
        (setq attr (opor-first-attribute-text obj))
        (setq height (opor-parse-real attr nil))
        (setq support (if height (opor-support-for-height height supports) nil))
        (if (not support)
          (setq support
            (vl-some
              '(lambda (item)
                 (if (= (cdr (assoc 'range item)) attr) item nil))
              supports)))
        (if support
          (progn
            (setq index (cdr (assoc 'index support)))
            (setq color (vla-get-Color obj))
            (if (and color (/= color 256))
              (setq counts (opor-color-count-add index color counts))))))))
  (setq result '())
  (foreach support supports
    (setq index (cdr (assoc 'index support)))
    (setq inferred (opor-best-color-for-index index counts))
    (setq result
      (cons
        (if (and inferred (= (cdr (assoc 'color support)) 256))
          (subst (cons 'color inferred) (assoc 'color support) support)
          support)
        result)))
  (reverse result))

(defun opor-table-nonzero-int-text (value)
  (if (and (numberp value) (/= (fix value) 0))
    (itoa (fix value))
    ""))

(defun opor-table-nonzero-real-text (value precision)
  (if (and (numberp value) (not (equal value 0.0 1e-9)))
    (rtos value 2 precision)
    ""))

;; Вставка блока таблицы с защитой COM. opor-insert-total-table рассчитывает на
;; nil при неудаче, чтобы включился запасной путь "новые таблицы -> легаси", но
;; необработанное исключение ActiveX пролетало мимо этой развилки и обрывало всю
;; команду на последнем шаге, уже после расчёта всех контуров сессии.
;; Регистрацию объекта делает вызывающий: в новых таблицах блок сначала
;; проверяется по числу атрибутов и при несовпадении удаляется без регистрации.
(defun opor-table-insert-block-safe (pt block-name / block)
  (setq block
    (vl-catch-all-apply 'vla-InsertBlock
      (list (opor-ms) (vlax-3d-point pt) block-name 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p block)
    (progn
      (opor-log
        (strcat "Не удалось вставить блок таблицы " (opor-string block-name)
                ": " (vl-catch-all-error-message block)))
      nil)
    block))

(defun opor-insert-legacy-total-table (session / line block-name pt block area qop gridlen step-x step-y support-index values counts variable-p tile-mode zfloor floor-out)
  (setq line (opor-session-get 'line))
  (setq block-name (opor-table-block-name line))
  (setq pt (opor-session-get 'table-point))
  (if (and block-name pt (opor-block-exists-p block-name)
           (setq block (opor-table-insert-block-safe pt block-name)))
    (progn
      (opor-register-created block "table")
      (setq area (opor-session-get 'area))
      (setq gridlen (opor-session-get 'lag-length-m))
      (setq step-x (opor-session-get 'step-x))
      (setq step-y (opor-session-get 'step-y))
      (setq support-index (opor-session-get 'support-index))
      (setq counts (opor-session-get 'support-counts))
      (setq variable-p (= (opor-session-get 'mode) "var-height"))
      (setq tile-mode (opor-session-get 'tile-mode))
      ;; ITOG: const = все опоры (qop); var = только сматченные с таблицей
      ;; (var_heght_fin: itogopor без ошибок)
      (setq qop
        (if variable-p
          (opor-counts-total counts)
          (opor-session-get 'support-count)))
      ;; CHPOL: введённая отметка, без zfloor-подъёма (arrfrm(3) - zfloor)
      (setq zfloor (opor-session-get 'zfloor))
      (if (not (numberp zfloor)) (setq zfloor 0.0))
      (setq floor-out (- (opor-session-get 'floor-height) zfloor))
      ;; видимость Доска/Плитка как dyn_blkset
      (opor-set-dynamic-visibility block (if (= tile-mode "p") "Плитка" "Доска"))
      (setq values
        (append
          (list
            (cons "AREA" (opor-table-nonzero-int-text (opor-round-half-even (/ area 1000000.0))))
            (cons "ITOG" (opor-table-nonzero-int-text qop))
            (cons "CHPOL" (if variable-p (rtos floor-out 2 0) "-"))
            (cons "DOSKA" (if variable-p (if (= tile-mode "d") (rtos (opor-session-get 'board-thickness) 2 0) "") "-"))
            (cons "LAG" (if variable-p (if (= tile-mode "d") (rtos (opor-session-get 'lag-thickness) 2 0) "") "-"))
            (cons "PLITKA" (if variable-p (if (= tile-mode "p") (rtos (opor-session-get 'tile-thickness) 2 0) "") "-"))
            (cons "VECTOR" (rtos step-x 2 0))
            (cons "PERP" (rtos step-y 2 0))
            (cons "LENGTH" (opor-table-nonzero-int-text gridlen))
            ;; S4: QC/QR = целых/обрезанных плиток (VBA qplw/qpltrim);
            ;; нулевые и не относящиеся к покрытию значения оставляем пустыми.
            (cons "QC" (if (= tile-mode "p") (opor-table-nonzero-int-text (opor-tiles-qc)) ""))
            (cons "QR" (if (= tile-mode "p") (opor-table-nonzero-int-text (opor-tiles-qr)) "")))
          (if variable-p
            (opor-number-tag-values-from-counts counts)
            (opor-number-tag-values support-index qop))))
      (opor-set-attribute-values block values)
      block)
    (progn
      (opor-log (strcat "Итоговая таблица не вставлена: не найден блок " (opor-string block-name)))
      nil)))

(defun opor-table-value-put (tag value values / pair)
  (setq pair (assoc tag values))
  (if pair
    (subst (cons tag value) pair values)
    (append values (list (cons tag value)))))

(defun opor-new-support-values (tags qop counts support-index variable-p / values index tag pair)
  (setq values '() index 1)
  (foreach tag tags
    (setq values (append values (list (cons tag ""))))
    (setq index (1+ index)))
  (if variable-p
    (foreach pair counts
      (setq index (car pair))
      (if (and (>= index 1) (<= index (length tags)))
        (setq values
          (opor-table-value-put
            (nth (1- index) tags)
            (opor-table-nonzero-int-text (cdr pair))
            values))))
    (if (and support-index (>= support-index 1)
             (<= support-index (length tags)))
      (setq values
        (opor-table-value-put
          (nth (1- support-index) tags)
          (opor-table-nonzero-int-text qop)
          values))))
  (append values
    (list (cons "SUP_TOTAL" (opor-table-nonzero-int-text qop)))))

(defun opor-new-3d-support-values (qop counts support-index variable-p)
  (opor-new-support-values
    *opor-new-3d-support-tags* qop counts support-index variable-p))

(defun opor-new-pro-support-values (qop counts support-index variable-p)
  (opor-new-support-values
    *opor-new-pro-support-tags* qop counts support-index variable-p))

(defun opor-new-table-floor-text (variable-p / zfloor floor-height)
  (if variable-p
    (progn
      (setq zfloor (opor-session-get 'zfloor))
      (if (not (numberp zfloor)) (setq zfloor 0.0))
      (setq floor-height (opor-session-get 'floor-height))
      (if (not (numberp floor-height)) (setq floor-height 0.0))
      (rtos (- floor-height zfloor) 2 0))
    "-"))

(defun opor-new-table-step-text (step-x step-y)
  (if (equal step-x step-y 1e-9)
    (rtos step-x 2 0)
    (strcat (rtos step-x 2 0) "×" (rtos step-y 2 0))))

(defun opor-new-table-number (key / value)
  (setq value (opor-session-get key))
  (if (numberp value)
    value
    (cdr (assoc key *opor-default-params*))))

(defun opor-new-table-dim-text (value)
  (if (equal value (float (fix value)) 1e-9)
    (itoa (fix value))
    (rtos value 2 2)))

(defun opor-new-table-covering-text (tile-mode tile-x tile-y / thickness)
  (if (= tile-mode "p")
    (progn
      (setq thickness (opor-new-table-number 'tile-thickness))
      (strcat
        "Плитка "
        (opor-new-table-dim-text tile-x) "×"
        (opor-new-table-dim-text tile-y) "×"
        (opor-new-table-dim-text thickness)))
    (progn
      (setq thickness (opor-new-table-number 'board-thickness))
      (strcat
        "Доска "
        (opor-new-table-dim-text (opor-new-table-number 'board-width)) "×"
        (opor-new-table-dim-text thickness) "×"
        (opor-new-table-dim-text (opor-new-table-number 'board-length))))))

(defun opor-new-table-lag-type-text (/ lagf prefix)
  (setq lagf (opor-session-get 'lag-fastener))
  (setq prefix
    (if (opor-fastener-name-has-p lagf "TOP")
      "Своя лага "
      "Frame "))
  (strcat
    prefix
    (opor-new-table-dim-text (opor-new-table-number 'lag-width)) "×"
    (opor-new-table-dim-text (opor-new-table-number 'lag-thickness))
    "мм"))

;; ТЗ п.6: один заголовок на весь запуск. При мультиконтуре центрируем его
;; над общим bbox всех успешно рассчитанных контуров.
(defun opor-drawing-title-bbox-union (a b / all aur bll bur)
  (cond
    ((not a) b)
    ((not b) a)
    (t
      (setq all (car a) aur (cadr a) bll (car b) bur (cadr b))
      (list
        (list (min (car all) (car bll))
              (min (cadr all) (cadr bll)) 0.0)
        (list (max (car aur) (car bur))
              (max (cadr aur) (cadr bur)) 0.0)))))

(defun opor-drawing-title-accumulate (boundary / bbox)
  (setq bbox (opor-bbox boundary))
  (if bbox
    (opor-session-set 'drawing-title-bbox
      (opor-drawing-title-bbox-union
        (opor-session-get 'drawing-title-bbox) bbox)))
  bbox)

(defun opor-drawing-title-text (/ line)
  (setq line (opor-session-get 'line))
  (strcat *opor-drawing-title-prefix*
    (if (member line '("3D" "PRO")) (strcat " " line) "")))

(defun opor-ensure-drawing-title-style ()
  (if (tblsearch "STYLE" *opor-drawing-title-style*)
    T
    (if
      (entmake
        (list
          '(0 . "STYLE")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbTextStyleTableRecord")
          (cons 2 *opor-drawing-title-style*)
          '(70 . 0)
          '(40 . 0.0)
          '(41 . 1.0)
          '(50 . 0.0)
          '(71 . 0)
          (cons 42 *opor-drawing-title-height*)
          (cons 3 *opor-drawing-title-font*)
          '(4 . "")))
      T
      (progn
        (opor-log "Не удалось создать текстовый стиль isocpeur.")
        nil))))

(defun opor-insert-drawing-title (session / bbox ll ur point text made en registered)
  (setq bbox (opor-session-get 'drawing-title-bbox))
  (if (and bbox (opor-ensure-drawing-title-style))
    (progn
      (setq ll (car bbox) ur (cadr bbox))
      (setq point
        (list (/ (+ (car ll) (car ur)) 2.0)
              (+ (cadr ur) *opor-drawing-title-offset*)
              0.0))
      (setq text (opor-drawing-title-text))
      (setq made
        (vl-catch-all-apply 'entmake
          (list
            (list
              '(0 . "TEXT")
              '(100 . "AcDbEntity")
              (cons 8 *opor-layer-contour*)
              '(100 . "AcDbText")
              (cons 10 point)
              (cons 40 *opor-drawing-title-height*)
              (cons 1 text)
              '(50 . 0.0)
              '(41 . 1.0)
              '(51 . 0.0)
              (cons 7 *opor-drawing-title-style*)
              '(71 . 0)
              '(72 . 1)
              (cons 11 point)
              '(210 0.0 0.0 1.0)
              '(73 . 2)))))
      (if (or (vl-catch-all-error-p made) (not made))
        (progn
          (opor-log "Заголовок чертежа не создан.")
          nil)
        (progn
          (setq en (entlast))
          (setq registered
            (vl-catch-all-apply 'opor-register-created
              (list en "drawing-title")))
          (if (or (vl-catch-all-error-p registered) (not registered))
            (progn
              (if en (entdel en))
              (opor-log "Заголовок чертежа не помечен XData OPOR.")
              nil)
            (progn
              (opor-log (strcat "Заголовок чертежа: " text "."))
              en)))))
    (progn
      (opor-log "Заголовок чертежа не создан: нет границ расчёта.")
      nil)))

;; Нижняя таблица расширяется по фактическому набору данных. Один общий
;; блок-строка с двумя атрибутами стекуется под BOARD/TILE; пустые строки
;; крепежа не создаются.
(defun opor-extra-row-text-style (/ block obj data style)
  (setq style "Standard")
  (setq block (tblobjname "BLOCK" *opor-new-board-params-block*))
  (if block
    (progn
      (setq obj (entnext block))
      (while obj
        (setq data (entget obj))
        (if (and (member (cdr (assoc 0 data)) '("MTEXT" "ATTDEF"))
                 (assoc 7 data))
          (setq style (cdr (assoc 7 data)) obj nil)
          (if (= (cdr (assoc 0 data)) "ENDBLK")
            (setq obj nil)
            (setq obj (entnext obj)))))))
  style)

(defun opor-extra-row-make-line (p1 p2)
  (entmake
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "0")
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p2)
      '(210 0.0 0.0 1.0))))

(defun opor-extra-row-make-attribute (tag point style / insert)
  ;; 10 — базовая точка TEXT, 11 — центр выравнивания; та же схема, что в
  ;; принятых BOARD/TILE ATTDEF.
  (setq insert (list (- (car point) 500.0) (- (cadr point) 125.0) 0.0))
  (entmake
    (list
      '(0 . "ATTDEF")
      '(100 . "AcDbEntity")
      '(8 . "0")
      '(100 . "AcDbText")
      (cons 10 insert)
      '(40 . 250.0)
      (cons 1 (strcat "[" tag "]"))
      '(50 . 0.0)
      '(41 . 1.0)
      '(51 . 0.0)
      (cons 7 style)
      '(71 . 0)
      '(72 . 1)
      (cons 11 point)
      '(210 0.0 0.0 1.0)
      '(100 . "AcDbAttributeDefinition")
      '(280 . 0)
      (cons 3 tag)
      (cons 2 tag)
      '(70 . 0)
      '(73 . 0)
      '(74 . 2)
      '(280 . 1))))

(defun opor-block-definition-attribute-count (name / block obj data count)
  (setq count 0)
  (setq block (tblobjname "BLOCK" name))
  (if block
    (progn
      (setq obj (entnext block))
      (while obj
        (setq data (entget obj))
        (if (= (cdr (assoc 0 data)) "ATTDEF")
          (setq count (1+ count)))
        (if (= (cdr (assoc 0 data)) "ENDBLK")
          (setq obj nil)
          (setq obj (entnext obj))))))
  count)

(defun opor-ensure-extra-row-block (/ style made)
  (if (opor-block-exists-p *opor-new-extra-row-block*)
    (= (opor-block-definition-attribute-count *opor-new-extra-row-block*) 2)
    (progn
      (setq style (opor-extra-row-text-style))
      (setq made
        (entmake
          (list
            '(0 . "BLOCK")
            '(100 . "AcDbEntity")
            '(8 . "0")
            '(100 . "AcDbBlockBegin")
            (cons 2 *opor-new-extra-row-block*)
            '(70 . 2)
            '(10 0.0 0.0 0.0))))
      (if made
        (progn
          (opor-extra-row-make-line '(0.0 0.0 0.0) '(10000.0 0.0 0.0))
          (opor-extra-row-make-line '(0.0 -800.0 0.0) '(10000.0 -800.0 0.0))
          (opor-extra-row-make-line '(0.0 0.0 0.0) '(0.0 -800.0 0.0))
          (opor-extra-row-make-line '(5500.0 0.0 0.0) '(5500.0 -800.0 0.0))
          (opor-extra-row-make-line '(10000.0 0.0 0.0) '(10000.0 -800.0 0.0))
          (opor-extra-row-make-attribute "LABEL" '(2750.0 -400.0 0.0) style)
          (opor-extra-row-make-attribute "VALUE" '(7750.0 -400.0 0.0) style)
          (entmake
            '((0 . "ENDBLK")
              (100 . "AcDbEntity")
              (8 . "0")
              (100 . "AcDbBlockEnd")))))
      (if (/= (opor-block-definition-attribute-count
                *opor-new-extra-row-block*) 2)
        (progn
          (opor-log "Не удалось создать блок дополнительной строки таблицы.")
          nil)
        T))))

(defun opor-new-param-fastener-row (name qty)
  (cons
    (strcat "Кол-во креплений " name)
    (opor-table-nonzero-int-text qty)))

;; Новые примеры заказчика задают не один фиксированный блок, а компоновку строк
;; по составу пола. Собираем всю нижнюю таблицу сверху вниз; строка FLOOR всегда
;; остаётся последней, как во всех подтверждённых вариантах.
(defun opor-new-param-row-values (variable-p / tile-mode area perimeter step-x step-y tile-x tile-y gridlen rows lagf tilef)
  (setq tile-mode (opor-session-get 'tile-mode))
  (setq area (opor-session-get 'area))
  (if (not (numberp area)) (setq area 0.0))
  (setq perimeter (opor-session-get 'perimeter-mm))
  (if (not (numberp perimeter)) (setq perimeter 0.0))
  (setq step-x (opor-session-get 'step-x))
  (setq step-y (opor-session-get 'step-y))
  (setq tile-x (opor-session-get 'tile-size-x))
  (setq tile-y (opor-session-get 'tile-size-y))
  (setq gridlen (opor-session-get 'lag-length-m))
  (if (not (numberp gridlen)) (setq gridlen 0))
  (setq rows
    (list
      (cons "Площадь объекта, м²"
        (opor-table-nonzero-int-text
          (opor-round-half-even (/ area 1000000.0))))
      (cons "Периметр объекта, м"
        (opor-table-nonzero-real-text (/ perimeter 1000.0) 2))
      (cons "Параметры финишного покрытия"
        (opor-new-table-covering-text tile-mode tile-x tile-y))
      (cons "Кол-во плитки с подрезкой"
        (if (= tile-mode "p")
          (opor-table-nonzero-int-text (opor-tiles-qr)) ""))
      (cons "Кол-во плитки без подрезки"
        (if (= tile-mode "p")
          (opor-table-nonzero-int-text (opor-tiles-qc)) ""))
      (cons "Кол-во доски с подрезкой"
        (if (= tile-mode "d")
          (opor-table-nonzero-int-text (opor-boards-qr)) ""))
      (cons "Кол-во доски без подрезки"
        (if (= tile-mode "d")
          (opor-table-nonzero-int-text (opor-boards-qc)) ""))))
  (if (opor-floor-uses-lag-param-table-p)
    (setq rows
      (append rows
        (list
          (cons "Параметры лаги" (opor-new-table-lag-type-text))
          (cons "Общая длина лаг, м"
            (opor-table-nonzero-int-text gridlen))
          (cons "Шаг опор вдоль лаги, мм" (rtos step-x 2 0))
          (cons "Шаг между лагами, мм" (rtos step-y 2 0)))))
    (setq rows
      (append rows
        (list
          (cons "Шаг опор, мм" (opor-new-table-step-text step-x step-y))))))
  (setq lagf (opor-session-get 'lag-fastener))
  (if lagf
    (setq rows
      (append rows
        (list
          (opor-new-param-fastener-row lagf
            (opor-session-get 'lag-fastener-count))))))
  (setq tilef (opor-session-get 'tile-fastener))
  (if tilef
    (setq rows
      (append rows
        (list
          (opor-new-param-fastener-row tilef
            (opor-session-get 'tile-fastener-count))))))
  (append rows
    (list
      (cons "Отметка чистого пола, мм"
        (opor-new-table-floor-text variable-p)))))

(defun opor-insert-extra-row (pt label value layer / block)
  (if (opor-ensure-extra-row-block)
    (progn
      (setq block
        (vl-catch-all-apply 'vla-InsertBlock
          (list (opor-ms) (vlax-3d-point pt)
            *opor-new-extra-row-block* 1.0 1.0 1.0 0.0)))
      (if (vl-catch-all-error-p block)
        nil
        (progn
          (if layer (vl-catch-all-apply 'vla-put-Layer (list block layer)))
          (opor-register-created block "table-extra-row")
          (opor-set-attribute-values block
            (list (cons "LABEL" label) (cons "VALUE" value)))
          block)))
    nil))

(defun opor-insert-new-param-table-rows (params-pt layer variable-p / y z rows pair inserted row)
  (setq y (cadr params-pt))
  (setq z (if (caddr params-pt) (caddr params-pt) 0.0))
  (setq rows (opor-new-param-row-values variable-p) inserted 0)
  (foreach pair rows
    (setq row
      (opor-insert-extra-row (list (car params-pt) y z)
        (car pair) (cdr pair) layer))
    (if row (setq inserted (1+ inserted)))
    (setq y (- y *opor-new-extra-row-height*)))
  (if (/= inserted (length rows))
    (opor-log
      (strcat "Добавлено строк таблицы параметров: "
        (itoa inserted) " из " (itoa (length rows)) ".")))
  inserted)

(defun opor-insert-new-support-tables (session line / pt params-pt support-block support-name support-tags params-offset qop support-index counts variable-p layer)
  (setq pt (opor-session-get 'table-point))
  (if (and pt (opor-import-new-table-blocks))
    (progn
      (if (= line "PRO")
        (setq support-name *opor-new-pro-support-table-block*
              support-tags *opor-new-pro-support-tags*
              params-offset *opor-new-pro-params-offset-y*)
        (setq support-name *opor-new-3d-support-table-block*
              support-tags *opor-new-3d-support-tags*
              params-offset *opor-new-3d-params-offset-y*))
      (setq params-pt
        (list (car pt) (+ (cadr pt) params-offset)
              (if (caddr pt) (caddr pt) 0.0)))
      (setq support-block (opor-table-insert-block-safe pt support-name))
      (if (and support-block
               (= (opor-block-reference-attribute-count support-block)
                  (1+ (length support-tags))))
        (progn
          (opor-register-created support-block "table-support")
          (setq support-index (opor-session-get 'support-index)
                counts (opor-session-get 'support-counts)
                variable-p (= (opor-session-get 'mode) "var-height"))
          (setq qop
            (if variable-p
              (opor-counts-total counts)
              (opor-session-get 'support-count)))
          (if (not (numberp qop)) (setq qop 0))
          (opor-set-attribute-values support-block
            (opor-new-support-values support-tags qop counts support-index variable-p))
          (setq layer
            (vl-catch-all-apply 'vla-get-Layer (list support-block)))
          (if (vl-catch-all-error-p layer) (setq layer nil))
          (opor-insert-new-param-table-rows params-pt layer variable-p)
          support-block)
        (progn
          ;; При сбое вставки причина уже записана в opor-table-insert-block-safe;
          ;; удалять и повторять сообщение про атрибуты тогда нечего.
          (if support-block
            (progn
              (vl-catch-all-apply 'vla-Delete (list support-block))
              (opor-log (strcat "Новые таблицы " line " не вставлены: в спецификации отсутствуют рабочие атрибуты."))))
          nil)))
    nil))

(defun opor-insert-new-3d-tables (session)
  (opor-insert-new-support-tables session "3D"))

(defun opor-insert-new-pro-tables (session)
  (opor-insert-new-support-tables session "PRO"))

(defun opor-insert-total-table (session / line block)
  (setq line (opor-session-get 'line))
  (cond
    ((= line "3D")
      (setq block (opor-insert-new-3d-tables session))
      (if block block (opor-insert-legacy-total-table session)))
    ((= line "PRO")
      (setq block (opor-insert-new-pro-tables session))
      (if block block (opor-insert-legacy-total-table session)))
    (t (opor-insert-legacy-total-table session))))

(princ)
