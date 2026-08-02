;;; make_height_check_etalons.lsp — ETS8 для h/check_height.
;;; Запускать в НОВОЙ рабочей копии шаблона с блоками opor_symb,
;;; otmetka_oporvb и проверкаvb3. Команда: ETS8.

(vl-load-com)

(defun ets8-doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun ets8-ms ()
  (vla-get-ModelSpace (ets8-doc)))

(defun ets8-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name)
        (cons 70 0)
        (cons 62 color)
        (cons 6 "Continuous")))))

(defun ets8-rect (x1 y1 x2 y2 layer)
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDbPolyline")
      (cons 90 4)
      (cons 70 1)
      (list 10 x1 y1)
      (list 10 x2 y1)
      (list 10 x2 y2)
      (list 10 x1 y2))))

(defun ets8-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (and (= (type value) 'safearray)
           (>= (vlax-safearray-get-u-bound value 1)
               (vlax-safearray-get-l-bound value 1)))
    (vlax-safearray->list value)
    '()))

(defun ets8-set-first-attribute (block text / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (ets8-list raw))
      (if atts (vla-put-TextString (car atts) text)))))

(defun ets8-insert (name x y layer color text / value block)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (ets8-ms) (vlax-3d-point (list x y 0.0))
        name 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    (progn
      (princ
        (strcat "\n[ETS8] ОШИБКА вставки " name ": "
          (vl-catch-all-error-message value)))
      nil)
    (progn
      (setq block value)
      (if layer (vl-catch-all-apply 'vla-put-Layer (list block layer)))
      (if color (vl-catch-all-apply 'vla-put-Color (list block color)))
      (if text (ets8-set-first-attribute block text))
      block)))

(defun ets8-blocks-ready-p (/ missing)
  (setq missing '())
  (foreach name '("opor_symb" "otmetka_oporvb" "проверкаvb3")
    (if (not (tblsearch "BLOCK" name))
      (setq missing (cons name missing))))
  (if missing
    (progn
      (princ "\n[ETS8] ОШИБКА: в шаблоне не найдены блоки:")
      (foreach name (reverse missing) (princ (strcat " " name)))
      nil)
    T))

(defun c:ETS8 ()
  (if (not (ets8-blocks-ready-p))
    nil
    (progn
      (ets8-ensure-layer "контур" 7)
      (ets8-ensure-layer "линии_высот" 8)
      (ets8-ensure-layer "линии_высот3" 8)
      (ets8-ensure-layer "опорыvb" 7)
      ;; Внешний контур не совпадает с областью — выбирать удобно и однозначно.
      (ets8-rect -500.0 -500.0 3500.0 5500.0 "контур")
      (ets8-rect 0.0 0.0 3000.0 5000.0 "линии_высот")
      ;; Отметки поверхности 0/10/20/10.
      (ets8-insert "otmetka_oporvb" 0.0 0.0 "линии_высот" 256 "0")
      (ets8-insert "otmetka_oporvb" 3000.0 0.0 "линии_высот" 256 "10")
      (ets8-insert "otmetka_oporvb" 3000.0 5000.0 "линии_высот" 256 "20")
      (ets8-insert "otmetka_oporvb" 0.0 5000.0 "линии_высот" 256 "10")
      ;; При floor=100 высоты опор в вершинах 100/90/80/90.
      (ets8-insert "opor_symb" 0.0 0.0 "опорыvb" 7 "100")
      (ets8-insert "opor_symb" 3000.0 0.0 "опорыvb" 7 "90")
      (ets8-insert "opor_symb" 3000.0 5000.0 "опорыvb" 7 "80")
      (ets8-insert "opor_symb" 0.0 5000.0 "опорыvb" 7 "90")
      ;; Проверяемая внутренняя опора: оранжевая, чтобы легко выбрать.
      (ets8-insert "opor_symb" 2000.0 1500.0 "опорыvb" 30 "90")
      (princ "\n[ETS8] Готово. Выбери внешний контур, затем ОРАНЖЕВУЮ опору")
      (princ "\n[ETS8] в точке 2000,1500; точка результата: 7000,5000.")
      (princ "\n[ETS8] Ожидание: floor=100, уровень=10, плоскость=10, блоков=4.")
      T))
  (princ))

(princ "\n[ETS8] make_height_check_etalons загружен. Команда: ETS8.")
(princ)
