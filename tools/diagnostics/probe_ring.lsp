;;; Независимая диагностика Ring. Команда RINGDUMP.

(vl-load-com)

(defun ringprobe-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray) (vlax-safearray->list value) '()))

(defun ringprobe-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun ringprobe-attrs (block / raw atts result att tag text)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (setq result '())
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (ringprobe-list raw))
      (foreach att atts
        (setq tag (vla-get-TagString att) text (vla-get-TextString att))
        (if (or (/= text "")
                (member (strcase tag) '("AREA" "ITOG" "CHPOL" "DOSKA" "LAG" "PLITKA" "VECTOR" "PERP")))
          (setq result (cons (cons tag text) result))))))
  (reverse result))

(defun c:RINGDUMP (/ doc ms obj name tables pair)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc) tables '())
  (vlax-for obj ms
    (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
      (progn
        (setq name (ringprobe-name obj))
        (if (member (strcase name) '("TABLE_TOTL_1" "TABLE_TOTL_3D" "TABLE_TOTL_PRO"))
          (setq tables (append tables (list obj)))))))
  (princ "\n========== RINGDUMP ==========")
  (princ (strcat "\nИтоговых таблиц: " (itoa (length tables))))
  (foreach obj tables
    (princ (strcat "\nTABLE " (ringprobe-name obj) " handle=" (vla-get-Handle obj)))
    (foreach pair (ringprobe-attrs obj)
      (princ (strcat "\n  " (car pair) "=" (cdr pair)))))
  (princ "\n========== КОНЕЦ RINGDUMP ==========")
  (princ))

(princ "\nprobe_ring загружен. Команда: RINGDUMP.")
(princ)
