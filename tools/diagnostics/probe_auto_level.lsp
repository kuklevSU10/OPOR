;;; Независимая диагностика ETS10. Команда AUTOLEVELDUMP.

(vl-load-com)

(defun autolevelprobe-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray) (vlax-safearray->list value) '()))

(defun autolevelprobe-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun autolevelprobe-attr (obj / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list obj)))
  (if (vl-catch-all-error-p raw)
    ""
    (progn
      (setq atts (autolevelprobe-list raw))
      (if atts (vla-get-TextString (car atts)) ""))))

(defun autolevelprobe-row-less-p (a b)
  (or (< (car a) (car b))
      (and (equal (car a) (car b) 1e-9) (< (cadr a) (cadr b)))))

(defun c:AUTOLEVELDUMP (/ doc ms obj pt rows row)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq rows '())
  (vlax-for obj ms
    (if (and
          (= (vla-get-ObjectName obj) "AcDbBlockReference")
          (= (strcase (autolevelprobe-name obj)) (strcase "otmetka_oporvb")))
      (progn
        (setq pt (autolevelprobe-list (vla-get-InsertionPoint obj)))
        (setq rows
          (cons
            (list (car pt) (cadr pt) (autolevelprobe-attr obj)
              (vla-get-Color obj) (vla-get-Layer obj))
            rows)))))
  (setq rows (vl-sort rows 'autolevelprobe-row-less-p))
  (princ "\n========== AUTOLEVELDUMP ==========")
  (princ (strcat "\nОтметок: " (itoa (length rows))))
  (foreach row rows
    (princ
      (strcat
        "\n  pt=" (rtos (nth 0 row) 2 0) "," (rtos (nth 1 row) 2 0)
        " text=" (nth 2 row)
        " color=" (itoa (nth 3 row))
        " layer=" (nth 4 row))))
  (princ "\n========== КОНЕЦ AUTOLEVELDUMP ==========")
  (princ))

(princ "\nprobe_auto_level загружен. Команда: AUTOLEVELDUMP.")
(princ)
