;;; ETS9 — эталон настоящего h/write_levl.
;;; Запускать в новом чертеже из шаблона с блоками slope и otmetka_oporvb.

(vl-load-com)

(defun ets9-doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun ets9-ms ()
  (vla-get-ModelSpace (ets9-doc)))

(defun ets9-set-first-attribute (block text / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq raw (vlax-variant-value raw))
      (if (= (type raw) 'safearray)
        (progn
          (setq atts (vlax-safearray->list raw))
          (if atts (vla-put-TextString (car atts) text)))))))

(defun ets9-insert (name x y color text / value block)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (ets9-ms) (vlax-3d-point (list x y 0.0)) name
        1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    (progn
      (princ
        (strcat "\n[ETS9] ОШИБКА вставки " name ": "
          (vl-catch-all-error-message value)))
      nil)
    (progn
      (setq block value)
      (if color (vl-catch-all-apply 'vla-put-Color (list block color)))
      (if text (ets9-set-first-attribute block text))
      block)))

(defun ets9-set-angle-zero (block / raw props prop name old result found)
  (setq found nil)
  (setq raw (vl-catch-all-apply 'vla-GetDynamicBlockProperties (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (if (= (type raw) 'variant) (setq raw (vlax-variant-value raw)))
      (setq props (if (= (type raw) 'safearray) (vlax-safearray->list raw) '()))
      (foreach prop props
        (setq name (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
        (if (and (not (vl-catch-all-error-p name)) (= name "Угол"))
          (progn
            (setq old (vla-get-Value prop))
            (setq result
              (vl-catch-all-apply
                'vla-put-Value
                (list prop
                  (if (= (type old) 'variant)
                    (vlax-make-variant 0.0 (vlax-variant-type old))
                    0.0))))
            (if (not (vl-catch-all-error-p result)) (setq found T)))))))
  found)

(defun ets9-ready-p (/ missing)
  (setq missing '())
  (foreach name '("slope" "otmetka_oporvb")
    (if (not (tblsearch "BLOCK" name))
      (setq missing (cons name missing))))
  (if missing
    (progn
      (princ "\n[ETS9] ОШИБКА: в шаблоне не найдены блоки:")
      (foreach name (reverse missing) (princ (strcat " " name)))
      nil)
    T))

(defun c:ETS9 (/ reference slope)
  (if (not (ets9-ready-p))
    nil
    (progn
      ;; Красная отметка — база; синий slope; оранжевые — четыре цели.
      (setq reference (ets9-insert "otmetka_oporvb" 0.0 0.0 1 "+100"))
      (setq slope (ets9-insert "slope" 500.0 500.0 5 "2.5%"))
      (if (and reference slope (ets9-set-angle-zero slope))
        (progn
          (ets9-insert "otmetka_oporvb" 1000.0 1000.0 30 "0")
          (ets9-insert "otmetka_oporvb" 2000.0 1500.0 30 "0")
          (ets9-insert "otmetka_oporvb" 3000.0 500.0 30 "0")
          (ets9-insert "otmetka_oporvb" -1000.0 -1000.0 30 "0")
          (vla-ZoomExtents (vlax-get-acad-object))
          (princ "\n[ETS9] Готово. h: красная отметка → синий slope → 4 оранжевые → Enter.")
          (princ "\n[ETS9] Ожидание VBA: +125, +137, +112, +125."))
        (princ "\n[ETS9] ОШИБКА: у slope не найдено/не задано свойство Угол."))))
  (princ))

(princ "\n[ETS9] make_write_level_etalons загружен. Команда: ETS9.")
(princ)
