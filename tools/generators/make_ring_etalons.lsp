;;; ETS11 — эталон Ring/m_ring.
;;; Строит один контур и опоры по первым трём строкам Level таблицы «Опоры».

(vl-load-com)

(defun ets11-value (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (cond
    ((= (type value) 'STR) value)
    ((numberp value) (rtos value 2 8))
    (t (vl-princ-to-string value))))

(defun ets11-strip (text / pos semi)
  (setq text (ets11-value text))
  (setq text (vl-string-subst "" "{" text))
  (setq text (vl-string-subst "" "}" text))
  (while (setq pos (vl-string-search "\\" text))
    (setq semi (vl-string-search ";" text pos))
    (setq text (strcat (substr text 1 pos) (if semi (substr text (+ semi 2)) ""))))
  text)

(defun ets11-cell (table row col / value)
  (setq value (vl-catch-all-apply 'vla-GetText (list table row col)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-GetCellValue (list table row col)))
      (if (vl-catch-all-error-p value) "" (ets11-strip value)))
    (ets11-strip value)))

(defun ets11-find-table (/ ms obj found)
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq found nil)
  (vlax-for obj ms
    (if (and (not found)
             (= (vla-get-ObjectName obj) "AcDbTable")
             (= (ets11-cell obj 0 0) "Опоры"))
      (setq found obj)))
  found)

;; Лёгкий «пример шаблона» содержит нужные блоки, но может не содержать саму
;; таблицу «Опоры». Для изолированного ETS11 создаём минимальную контрольную.
(defun ets11-ensure-support-table (/ ms value table rows)
  (if (not (ets11-find-table))
    (progn
      (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq value
        (vl-catch-all-apply
          'vla-AddTable
          (list ms (vlax-3d-point '(5000.0 2000.0 0.0)) 5 3 250.0 900.0)))
      (if (vl-catch-all-error-p value)
        nil
        (progn
          (setq table value)
          (setq rows
            '((0 0 "Опоры")
              (1 0 "Наименование") (1 1 "Диапазон высот") (1 2 "Цвет")
              (2 0 "LLOW12") (2 1 "<17") (2 2 "221")
              (3 0 "LLOW20") (3 1 "17-25") (3 2 "2")
              (4 0 "L0") (4 1 "25-35") (4 2 "1")))
          (foreach item rows
            (vl-catch-all-apply
              'vla-SetText
              (list table (car item) (cadr item) (caddr item))))
          ;; В production-таблице цвета хранятся числовыми Variant. Если оставить
          ;; строки, VBA сравнивает их с AcColor как разные типы и получает 0.
          (vl-catch-all-apply
            'vla-SetCellValue
            (list table 2 2 (vlax-make-variant 221 vlax-vbLong)))
          (vl-catch-all-apply
            'vla-SetCellValue
            (list table 3 2 (vlax-make-variant 2 vlax-vbLong)))
          (vl-catch-all-apply
            'vla-SetCellValue
            (list table 4 2 (vlax-make-variant 1 vlax-vbLong)))
          table)))))

(defun ets11-real (text / clean value)
  (setq clean (vl-string-translate "," "." (vl-string-trim " " text)))
  (setq clean (vl-string-subst "" "*" clean))
  (setq clean (vl-string-subst "" "+" clean))
  (setq value (vl-catch-all-apply 'read (list clean)))
  (if (or (vl-catch-all-error-p value) (not (numberp value))) nil value))

(defun ets11-max (range / clean pos)
  (setq clean (vl-string-translate "," "." (vl-string-trim " " range)))
  (setq clean (vl-string-subst "" "*" clean))
  (cond
    ((setq pos (vl-string-search "<" clean))
      (ets11-real (substr clean (+ pos 2))))
    ((setq pos (vl-string-search "-" clean))
      (ets11-real (substr clean (+ pos 2))))
    (t nil)))

(defun ets11-int (text / value)
  (setq value (ets11-real text))
  (if value (fix value) 256))

(defun ets11-rows (/ table rows row name range color maxh result)
  (setq table (ets11-find-table) result '())
  (if table
    (progn
      (setq rows (vla-get-Rows table) row 2)
      (while (and (< row rows) (< (length result) 3))
        (setq name (vl-string-trim " " (ets11-cell table row 0)))
        (setq range (vl-string-trim " " (ets11-cell table row 1)))
        (setq color (ets11-int (ets11-cell table row 2)))
        (setq maxh (ets11-max range))
        (if (and (/= name "") maxh
                 (not (wcmatch (strcase name) "*3D*"))
                 (not (wcmatch (strcase name) "*PRO*")))
          (setq result (append result (list (list name maxh color)))))
        (setq row (1+ row)))))
  result)

(defun ets11-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name) (cons 70 0) (cons 62 color) (cons 6 "Continuous")))))

(defun ets11-rect (x1 y1 x2 y2 layer)
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity") (cons 8 layer)
      (cons 100 "AcDbPolyline") (cons 90 4) (cons 70 1)
      (list 10 x1 y1) (list 10 x2 y1)
      (list 10 x2 y2) (list 10 x1 y2))))

(defun ets11-set-first-attribute (block text / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq raw (vlax-variant-value raw))
      (if (= (type raw) 'safearray)
        (progn
          (setq atts (vlax-safearray->list raw))
          (if atts (vla-put-TextString (car atts) text)))))))

(defun ets11-insert-support (x y color height / ms value block)
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list ms (vlax-3d-point (list x y 0.0)) "opor_symb" 1.0 1.0 1.0 0.0)))
  (if (not (vl-catch-all-error-p value))
    (progn
      (setq block value)
      (vla-put-Layer block "опорыvb")
      (vla-put-Color block color)
      (ets11-set-first-attribute block (itoa (fix height)))
      block)))

(defun c:ETS11 (/ rows idx item x j expected)
  (ets11-ensure-support-table)
  (setq rows (ets11-rows))
  (cond
    ((not (tblsearch "BLOCK" "opor_symb"))
      (princ "\n[ETS11] ОШИБКА: нет определения блока opor_symb."))
    ((< (length rows) 3)
      (princ "\n[ETS11] ОШИБКА: в таблице «Опоры» не найдено три строки Level."))
    (t
      (ets11-ensure-layer "контур" 7)
      (ets11-ensure-layer "опорыvb" 7)
      (ets11-rect -200.0 -200.0 4000.0 2200.0 "контур")
      (setq idx 0 expected 0)
      (foreach item rows
        (setq x (+ 500.0 (* idx 1200.0)))
        (setq j 0)
        ;; Для строк 1/2/3 создаются соответственно 1/2/3 совпадающие опоры.
        (while (<= j idx)
          (ets11-insert-support x (+ 400.0 (* j 300.0)) (nth 2 item) (- (nth 1 item) j))
          (setq expected (1+ expected) j (1+ j)))
        ;; Высота Hmax-10 не должна попасть в Ring.
        (ets11-insert-support x 1700.0 (nth 2 item) (- (nth 1 item) 10.0))
        (princ
          (strcat "\n[ETS11] " (car item) ": цвет=" (itoa (nth 2 item))
            ", Hmax=" (itoa (fix (nth 1 item))) ", ожидается=" (itoa (1+ idx))))
        (setq idx (1+ idx)))
      (vla-ZoomExtents (vlax-get-acad-object))
      (princ (strcat "\n[ETS11] Готово: выбери контур, точку таблицы и Enter; ITOG=" (itoa expected) "."))))
  (princ))

(princ "\n[ETS11] make_ring_etalons загружен. Команда: ETS11.")
(princ)
