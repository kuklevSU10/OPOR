;;; probe_height_check.lsp — независимая A/B-диагностика ETS8.
;;; Команда: HEIGHTDUMP. Чертёж не изменяет.

(vl-load-com)

(defun heightprobe-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (and (= (type value) 'safearray)
           (>= (vlax-safearray-get-u-bound value 1)
               (vlax-safearray-get-l-bound value 1)))
    (vlax-safearray->list value)
    '()))

(defun heightprobe-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun heightprobe-attrs (obj / raw result)
  (setq result '())
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list obj)))
  (if (not (vl-catch-all-error-p raw))
    (foreach att (heightprobe-list raw)
      (setq result
        (cons
          (cons (strcase (vla-get-TagString att)) (vla-get-TextString att))
          result))))
  result)

(defun heightprobe-attr (tag attrs / pair)
  (setq pair (assoc tag attrs))
  (if pair (cdr pair) ""))

(defun heightprobe-point (obj / raw)
  (setq raw (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p raw) '(0.0 0.0 0.0) (heightprobe-list raw)))

(defun heightprobe-block-row (obj / pt attrs)
  (setq pt (heightprobe-point obj))
  (setq attrs (heightprobe-attrs obj))
  (list
    (car pt) (cadr pt)
    (vla-get-XScaleFactor obj)
    (vla-get-Color obj)
    (heightprobe-attr "LEV" attrs)
    (heightprobe-attr "H" attrs)))

(defun heightprobe-row-less-p (a b)
  (or (< (cadr a) (cadr b))
      (and (equal (cadr a) (cadr b) 1e-9) (< (car a) (car b)))))

(defun c:HEIGHTDUMP (/ doc ms obj name rows level-count tri-count row)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq rows '() level-count 0 tri-count 0)
  (vlax-for obj ms
    (cond
      ((= (vla-get-ObjectName obj) "AcDbBlockReference")
        (setq name (heightprobe-name obj))
        (if (= (strcase name) (strcase "проверкаvb3"))
          (setq rows (cons (heightprobe-block-row obj) rows))))
      ((= (vla-get-ObjectName obj) "AcDbPolyline")
        (cond
          ((= (strcase (vla-get-Layer obj)) (strcase "линии_высот"))
            (setq level-count (1+ level-count)))
          ((= (strcase (vla-get-Layer obj)) (strcase "линии_высот3"))
            (setq tri-count (1+ tri-count)))))))
  (setq rows (vl-sort rows 'heightprobe-row-less-p))
  (princ "\n========== HEIGHTDUMP ==========")
  (princ (strcat "\nБлоков проверкаvb3: " (itoa (length rows))))
  (foreach row rows
    (princ
      (strcat
        "\n  pt=" (rtos (nth 0 row) 2 0) "," (rtos (nth 1 row) 2 0)
        " scale=" (rtos (nth 2 row) 2 2)
        " color=" (itoa (nth 3 row))
        " LEV=" (nth 4 row)
        " H=" (nth 5 row))))
  (princ (strcat "\nПолилиний линии_высот: " (itoa level-count)))
  (princ (strcat "\nПолилиний линии_высот3: " (itoa tri-count)))
  (princ "\n========== КОНЕЦ HEIGHTDUMP ==========")
  (princ))

(princ "\nprobe_height_check загружен. Команда: HEIGHTDUMP.")
(princ)
