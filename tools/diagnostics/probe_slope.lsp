;;; probe_slope.lsp — независимая A/B-диагностика VBA/LISP для ETS7.
;;; Команда: SLOPEDUMP. Ничего в чертеже не изменяет.

(vl-load-com)

(defun slopeprobe-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (and (= (type value) 'safearray)
           (>= (vlax-safearray-get-u-bound value 1)
               (vlax-safearray-get-l-bound value 1)))
    (vlax-safearray->list value)
    '()))

(defun slopeprobe-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun slopeprobe-first-attr (obj / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list obj)))
  (if (vl-catch-all-error-p raw)
    ""
    (progn
      (setq atts (slopeprobe-list raw))
      (if atts (vla-get-TextString (car atts)) ""))))

(defun slopeprobe-inc (key counts / pair)
  (setq pair (assoc key counts))
  (if pair
    (subst (cons key (1+ (cdr pair))) pair counts)
    (cons (cons key 1) counts)))

(defun slopeprobe-print-counts (title counts)
  (princ (strcat "\n--- " title " ---"))
  (if counts
    (foreach pair (vl-sort counts '(lambda (a b) (< (car a) (car b))))
      (princ (strcat "\n  " (car pair) " = " (itoa (cdr pair)))))
    (princ "\n  нет")))

(defun slopeprobe-table-attrs (table / raw atts rows)
  (setq rows '())
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list table)))
  (if (not (vl-catch-all-error-p raw))
    (foreach att (slopeprobe-list raw)
      (setq rows
        (cons
          (cons (strcase (vla-get-TagString att)) (vla-get-TextString att))
          rows))))
  (vl-sort rows '(lambda (a b) (< (car a) (car b)))))

(defun slopeprobe-rgb (obj / tc r g b)
  (setq tc (vl-catch-all-apply 'vla-get-TrueColor (list obj)))
  (if (vl-catch-all-error-p tc)
    "?"
    (progn
      (setq r (vl-catch-all-apply 'vla-get-Red (list tc)))
      (setq g (vl-catch-all-apply 'vla-get-Green (list tc)))
      (setq b (vl-catch-all-apply 'vla-get-Blue (list tc)))
      (if (or (vl-catch-all-error-p r)
              (vl-catch-all-error-p g)
              (vl-catch-all-error-p b))
        "?"
        (strcat (itoa r) "," (itoa g) "," (itoa b))))))

;; Тот же источник и та же сортировка, что у VBA v_read_blk_slope и порта.
(defun c:SLOPECOLORS (/ doc blocks definition value circles obj center item percent)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks doc))
  (setq value (vl-catch-all-apply 'vla-Item (list blocks "table_slope")))
  (if (vl-catch-all-error-p value)
    (princ "\nSLOPECOLORS: блок table_slope не найден.")
    (progn
      (setq definition value circles '())
      (vlax-for obj definition
        (if (= (vla-get-ObjectName obj) "AcDbCircle")
          (progn
            (setq center
              (slopeprobe-list
                (vl-catch-all-apply 'vla-get-Center (list obj))))
            (if center
              (setq circles
                (cons
                  (list (cadr center) (vla-get-Color obj)
                        (slopeprobe-rgb obj) (vla-get-Layer obj))
                  circles))))))
      (setq circles
        (vl-sort circles '(lambda (a b) (> (car a) (car b)))))
      (princ
        (strcat "\n========== SLOPECOLORS: окружностей="
          (itoa (length circles)) " =========="))
      (setq percent 2)
      (foreach item circles
        (princ
          (strcat
            "\nP" (itoa percent)
            " y=" (rtos (car item) 2 2)
            " ACI=" (itoa (cadr item))
            " RGB=" (caddr item)
            " layer=" (cadddr item)))
        (setq percent (1+ percent)))
      (princ "\n========== КОНЕЦ SLOPECOLORS ==========")))
  (princ))

(defun c:SLOPEDUMP (/ doc ms obj name key slopes supports tables areas table-index)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq slopes '() supports '() tables '() areas 0)
  (vlax-for obj ms
    (cond
      ((and (= (vla-get-ObjectName obj) "AcDbPolyline")
            (= (strcase (vla-get-Layer obj)) (strcase "линии_высот")))
        (setq areas (1+ areas)))
      ((= (vla-get-ObjectName obj) "AcDbBlockReference")
        (setq name (slopeprobe-name obj))
        (cond
          ((= (strcase name) "SLOPE")
            (setq key
              (strcat
                "text=" (slopeprobe-first-attr obj)
                " | layer=" (vla-get-Layer obj)
                " | color=" (itoa (vla-get-Color obj))))
            (setq slopes (slopeprobe-inc key slopes)))
          ((member (strcase name) '("OPOR_SYMB" "OPOR_SYMB3D" "OPOR_SYMBPRO"))
            (setq key
              (strcat name " | color=" (itoa (vla-get-Color obj))))
            (setq supports (slopeprobe-inc key supports)))
          ((= (strcase name) "TABLE_SLOPE")
            (setq tables (cons obj tables)))))))
  (princ "\n========== SLOPEDUMP ==========")
  (princ (strcat "\nОбластей линии_высот: " (itoa areas)))
  (slopeprobe-print-counts "Блоки slope" slopes)
  (slopeprobe-print-counts "Опоры" supports)
  (princ (strcat "\n--- Вставки table_slope: " (itoa (length tables)) " ---"))
  (setq table-index 1)
  (foreach table (reverse tables)
    (princ (strcat "\n  [" (itoa table-index) "]"))
    (foreach pair (slopeprobe-table-attrs table)
      (if (/= (cdr pair) "")
        (princ (strcat " " (car pair) "=" (cdr pair)))))
    (setq table-index (1+ table-index)))
  (princ "\n========== КОНЕЦ SLOPEDUMP ==========")
  (princ))

(princ "\nprobe_slope загружен. Команды: SLOPEDUMP, SLOPECOLORS.")
(princ)
