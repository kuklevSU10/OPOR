;;; Независимая диагностика ETS9 для LISP/VBA A/B. Команда WRITELEVELDUMP.

(vl-load-com)

(defun writeprobe-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray) (vlax-safearray->list value) '()))

(defun writeprobe-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun writeprobe-attr (obj / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list obj)))
  (if (vl-catch-all-error-p raw)
    ""
    (progn
      (setq atts (writeprobe-list raw))
      (if atts (vla-get-TextString (car atts)) ""))))

(defun writeprobe-point (obj / value pts)
  (setq value (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p value)
    '(0.0 0.0 0.0)
    (progn
      (setq pts (writeprobe-list value))
      (if pts pts '(0.0 0.0 0.0)))))

(defun writeprobe-angle (obj / raw props prop name value angle)
  (setq angle 0.0)
  (setq raw (vl-catch-all-apply 'vla-GetDynamicBlockProperties (list obj)))
  (if (not (vl-catch-all-error-p raw))
    (foreach prop (writeprobe-list raw)
      (setq name (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
      (if (and (not (vl-catch-all-error-p name)) (= name "Угол"))
        (progn
          (setq value (vl-catch-all-apply 'vla-get-Value (list prop)))
          (if (not (vl-catch-all-error-p value))
            (progn
              (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
              (if (numberp value) (setq angle value))))))))
  angle)

(defun writeprobe-row-less-p (a b)
  (or (< (car a) (car b))
      (and (equal (car a) (car b) 1e-9) (< (cadr a) (cadr b)))))

(defun c:WRITELEVELDUMP (/ doc ms obj name pt marks slopes row)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq marks '() slopes '())
  (vlax-for obj ms
    (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
      (progn
        (setq name (strcase (writeprobe-name obj)))
        (cond
          ((= name (strcase "otmetka_oporvb"))
            (setq pt (writeprobe-point obj))
            (setq marks
              (cons (list (car pt) (cadr pt) (writeprobe-attr obj) (vla-get-Color obj)) marks)))
          ((= name (strcase "slope"))
            (setq pt (writeprobe-point obj))
            (setq slopes
              (cons
                (list (car pt) (cadr pt) (writeprobe-attr obj)
                  (vla-get-Rotation obj) (writeprobe-angle obj))
                slopes)))))))
  (setq marks (vl-sort marks 'writeprobe-row-less-p))
  (princ "\n========== WRITELEVELDUMP ==========")
  (princ (strcat "\nОтметок: " (itoa (length marks))))
  (foreach row marks
    (princ
      (strcat
        "\n  pt=" (rtos (nth 0 row) 2 0) "," (rtos (nth 1 row) 2 0)
        " text=" (nth 2 row) " color=" (itoa (nth 3 row)))))
  (princ (strcat "\nSlope: " (itoa (length slopes))))
  (foreach row slopes
    (princ
      (strcat
        "\n  pt=" (rtos (nth 0 row) 2 0) "," (rtos (nth 1 row) 2 0)
        " text=" (nth 2 row)
        " rotation=" (rtos (nth 3 row) 2 6)
        " angle=" (rtos (nth 4 row) 2 6))))
  (princ "\n========== КОНЕЦ WRITELEVELDUMP ==========")
  (princ))

(princ "\nprobe_write_level загружен. Команда: WRITELEVELDUMP.")
(princ)
