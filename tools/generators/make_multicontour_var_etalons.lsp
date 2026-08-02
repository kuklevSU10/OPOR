;;; ETS14 — два независимых Var-контура для проверки общих накопителей.

(vl-load-com)

(defun ets14-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name) (cons 70 0) (cons 62 color) (cons 6 "Continuous")))))

(defun ets14-rect (x1 y1 x2 y2 layer)
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity") (cons 8 layer)
      (cons 100 "AcDbPolyline") (cons 90 4) (cons 70 1)
      (list 10 x1 y1) (list 10 x2 y1)
      (list 10 x2 y2) (list 10 x1 y2))))

(defun ets14-variant-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray) (vlax-safearray->list value) '()))

(defun ets14-insert-mark (x y text / doc ms value block raw atts)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list ms (vlax-3d-point (list x y 0.0))
            "otmetka_oporvb" 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq block value)
      (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
      (if (not (vl-catch-all-error-p raw))
        (progn
          (setq atts (ets14-variant-list raw))
          (if atts (vla-put-TextString (car atts) text))))
      block)))

(defun ets14-add-level-set (dx)
  ;; Область высот создаётся раньше внешнего контура: совпадающий внешний
  ;; контур остаётся верхним объектом и его проще выбрать кликом.
  (ets14-rect dx 0.0 (+ dx 3000.0) 5000.0 "линии_высот")
  (ets14-insert-mark dx 0.0 "0")
  (ets14-insert-mark (+ dx 3000.0) 0.0 "10")
  (ets14-insert-mark (+ dx 3000.0) 5000.0 "20")
  (ets14-insert-mark dx 5000.0 "10")
  (ets14-rect dx 0.0 (+ dx 3000.0) 5000.0 "контур"))

(defun c:ETS14 ()
  (ets11-ensure-support-table)
  (ets14-ensure-layer "контур" 7)
  (ets14-ensure-layer "линии_высот" 8)
  (ets14-add-level-set 0.0)
  (ets14-add-level-set 6000.0)
  (vla-ZoomExtents (vlax-get-acad-object))
  (princ "\n[ETS14] Готово: два Var-контура S1, две области высот и 8 отметок.")
  (princ "\n[ETS14] Var: lev, 600/600, R100, доска, floor=101, доска=27, лага=49, сдв. лага=0, vect.")
  (princ "\n[ETS14] 1-й: base=0,0; dir=@1<0; table=11000,5000.")
  (princ "\n[ETS14] 2-й: base=6000,0; dir=@1<0; затем Enter завершает цикл.")
  (princ "\n[ETS14] Общие инварианты: 120 опор, AREA=30, LENGTH=60, CHPOL=101, одна таблица; распределение строк сравнивается LISP/VBA.")
  (princ))

(princ "\n[ETS14] make_multicontour_var_etalons загружен. Команда: ETS14.")
(princ)
