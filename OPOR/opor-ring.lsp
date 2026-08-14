;;; OPOR Ring/m_ring: выбор контура и сокращённая ведомость опор.
;;; VBA при нескольких контурах перезаписывает результат на каждом проходе;
;;; итоговая таблица поэтому отражает последний обработанный контур.

(defun opor-ring-line-for-block (block / name)
  (setq name (strcase (opor-effective-block-name block)))
  (cond
    ((= name "OPOR_SYMB") "lev")
    ((= name "OPOR_SYMB3D") "3D")
    ((= name "OPOR_SYMBPRO") "PRO")
    (t nil)))

(defun opor-ring-support-block-p (obj)
  (and
    (= (opor-obj-name obj) "AcDbBlockReference")
    (opor-ring-line-for-block obj)))

(defun opor-ring-crossing-supports (boundary / bbox objects result obj)
  (setq bbox (opor-bbox boundary))
  (setq objects (if bbox (opor-slope-crossing-blocks bbox) '()))
  (setq result '())
  (foreach obj objects
    (if (opor-ring-support-block-p obj)
      (setq result (cons obj result))))
  (reverse result))

;; m_ring: Hmin всегда Hmax-9, нижняя граница исходного диапазона игнорируется.
(defun opor-ring-row-for-block (block supports / color text height found row maxh)
  (setq color (vla-get-Color block))
  (setq text (opor-first-attribute-text block))
  (setq height (opor-parse-real text nil))
  (if (numberp height) (setq height (opor-round-half-even height))) ; VBA CInt
  (setq found nil)
  (foreach row supports
    (if (not found)
      (progn
        (setq maxh (cdr (assoc 'max row)))
        (if (and
              (numberp height)
              (numberp maxh)
              (= color (cdr (assoc 'color row)))
              (>= height (- maxh 9.0))
              (<= height maxh))
          (setq found row)))))
  found)

;; Запись: (цвет количество наименование). VBA group группирует только по цвету.
(defun opor-ring-count-add (row counts / color pair replacement)
  (setq color (cdr (assoc 'color row)))
  (setq pair (assoc color counts))
  (if pair
    (progn
      (setq replacement (list color (1+ (cadr pair)) (caddr pair)))
      (subst replacement pair counts))
    (append counts (list (list color 1 (cdr (assoc 'name row)))))))

(defun opor-ring-analyze (blocks / line supports counts total row block)
  (setq line (if blocks (opor-ring-line-for-block (car blocks)) nil))
  (setq supports (if line (opor-read-supports line) '()))
  (setq counts '() total 0)
  (foreach block blocks
    (setq row (opor-ring-row-for-block block supports))
    (if row
      (progn
        (setq counts (opor-ring-count-add row counts))
        (setq total (1+ total)))))
  (list
    (cons 'line line)
    (cons 'blocks (length blocks))
    (cons 'counts counts)
    (cons 'total total)))

(defun opor-ring-tag-values (counts / tags values tag item name)
  (setq tags
    '("LLOW12" "LLOW20" "L0" "L1" "L2" "L3" "L4" "L5" "L6" "L7"
      "L1_3D" "L2_3D" "L3_3D" "L4_3D" "L5_3D"
      "L6_3D" "L7_3D" "L8_3D" "L9_3D"))
  (setq values '())
  (foreach tag tags
    (setq item nil)
    (foreach candidate counts
      (if (not item)
        (progn
          (setq name (caddr candidate))
          (if (and name (vl-string-search tag name))
            (setq item candidate)))))
    (if item
      (setq values (cons (cons tag (itoa (cadr item))) values))))
  (reverse values))

(defun opor-ring-insert-table (point result / line block-name value block values pair)
  (setq line (cdr (assoc 'line result)))
  (setq block-name (opor-table-block-name line))
  (if (and point block-name (opor-block-exists-p block-name))
    (progn
      (setq value
        (vl-catch-all-apply
          'vla-InsertBlock
          (list (opor-ms) (vlax-3d-point point) block-name 1.0 1.0 1.0 0.0)))
      (if (vl-catch-all-error-p value)
        (progn
          (opor-alert
            (strcat "Не удалось вставить " block-name ".\n"
              (vl-catch-all-error-message value)))
          nil)
        (progn
          (setq block value)
          (setq values
            (append
              (list
                (cons "AREA" "")
                (cons "ITOG"
                  (opor-table-nonzero-int-text
                    (cdr (assoc 'total result))))
                (cons "CHPOL" "")
                (cons "DOSKA" "")
                (cons "LAG" "")
                (cons "PLITKA" "")
                (cons "VECTOR" "")
                (cons "PERP" ""))
              (opor-empty-number-tag-values)))
          (foreach pair (opor-ring-tag-values (cdr (assoc 'counts result)))
            (setq values
              (opor-table-value-put (car pair) (cdr pair) values)))
          (if (and (opor-set-attribute-values block values)
                   (opor-register-created block "ring-table"))
            block
            (progn
              (opor-delete-object block)
              (opor-alert "Ведомость Ring не заполнена или не помечена XData OPOR.")
              nil)))))
    (progn
      (opor-alert (strcat "Не найден блок итоговой таблицы " (opor-string block-name) "."))
      nil)))

(defun opor-ring-run (/ boundary table-point first-p blocks result done table)
  (if (not (opor-find-support-table))
    (progn
      (opor-alert "Не найдена таблица высот опор.")
      nil)
    (progn
      (opor-view-save)
      (setq first-p T done nil result nil table-point nil)
      (while (not done)
        (setq boundary (opor-slope-pick-boundary (not first-p) T))
        (cond
          ((not boundary) (setq done T))
          ((= boundary 'invalid) nil)
          (t
            (if first-p
              (progn
                (setq table-point
                  (getpoint "\nУкажите точку верхнего левого угла таблицы: "))
                (if table-point
                  (setq first-p nil)
                  (setq done T))))
            (if (and (not first-p) (not done))
              (progn
                (opor-zoom-to-boundary boundary)
                (setq blocks (opor-ring-crossing-supports boundary))
                (if blocks
                  (progn
                    ;; Точное поведение VBA: результат предыдущего контура заменяется.
                    (setq result (opor-ring-analyze blocks))
                    (opor-log
                      (strcat
                        "Ring: блоков=" (itoa (cdr (assoc 'blocks result)))
                        ", учтено=" (itoa (cdr (assoc 'total result)))
                        ", линейка=" (opor-string (cdr (assoc 'line result))) ".")))
                  (opor-alert "Не найдены блоки опор.")))))))
      (if (and table-point result (cdr (assoc 'line result)))
        (progn
          (setq table (opor-ring-insert-table table-point result))
          (if table
            (progn
              (opor-session-set 'ring-total (cdr (assoc 'total result)))
              (opor-session-set 'ring-line (cdr (assoc 'line result)))
              (opor-alert
                (strcat "Ring завершён. Учтено опор: "
                  (itoa (cdr (assoc 'total result))) "."))
              T)
            nil))
        nil))))

(defun opor-command-ring ()
  (opor-init-session)
  (opor-ring-run))

(princ)
