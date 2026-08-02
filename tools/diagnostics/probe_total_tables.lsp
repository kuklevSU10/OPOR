;;; Независимый снимок вставок table_totl_* для LISP/VBA A/B. Команда TOTALDUMP.

(vl-load-com)

(defun totaldump-object-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun totaldump-variant-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray) (vlax-safearray->list value) '()))

(defun totaldump-attributes (block / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (totaldump-variant-list raw))
      (foreach att atts
        (princ
          (strcat
            "\n    " (vla-get-TagString att)
            " = " (vla-get-TextString att)))))))

(defun c:TOTALDUMP (/ doc ms obj name count)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq count 0)
  (princ "\n========== TOTAL TABLE DUMP ==========")
  (vlax-for obj ms
    (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
      (progn
        (setq name (totaldump-object-name obj))
        (if (wcmatch name "table_totl_*")
          (progn
            (setq count (1+ count))
            (princ
              (strcat
                "\n--- TABLE #" (itoa count) " ---"
                "\n    block = " name
                "\n    layer = " (vla-get-Layer obj)))
            (totaldump-attributes obj))))))
  (princ (strcat "\n--- COUNT = " (itoa count) " ---"))
  (princ "\n======================================")
  (princ))

(princ "\nprobe_total_tables загружен. Команда: TOTALDUMP.")
(princ)
