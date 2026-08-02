;;; ETS13 — два независимых S1-контура для проверки общего цикла starts/fin.

(vl-load-com)

(defun ets13-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name) (cons 70 0) (cons 62 color) (cons 6 "Continuous")))))

(defun ets13-rect (x1 y1 x2 y2 layer)
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity") (cons 8 layer)
      (cons 100 "AcDbPolyline") (cons 90 4) (cons 70 1)
      (list 10 x1 y1) (list 10 x2 y1)
      (list 10 x2 y2) (list 10 x1 y2))))

(defun c:ETS13 ()
  ;; Загружается после make_ring_etalons.lsp, поэтому эта функция доступна и
  ;; добавляет минимальную числовую таблицу «Опоры» в лёгкий тестовый DWG.
  (ets11-ensure-support-table)
  (ets13-ensure-layer "контур" 7)
  (ets13-rect 0.0 0.0 3000.0 5000.0 "контур")
  (ets13-rect 6000.0 0.0 9000.0 5000.0 "контур")
  (vla-ZoomExtents (vlax-get-acad-object))
  (princ "\n[ETS13] Готово: два контура 3000x5000, второй сдвинут на 6000 мм.")
  (princ "\n[ETS13] Const 600/600, R100, vect, доска, без крепежа.")
  (princ "\n[ETS13] 1-й: base=0,0; dir=@1<0; table=11000,5000.")
  (princ "\n[ETS13] 2-й: base=6000,0; dir=@1<0; затем Enter завершает цикл.")
  (princ "\n[ETS13] Ожидание: контуров=2, опор=120, лаг=20/60000 мм, AREA=30, LENGTH=60, одна table_totl_1.")
  (princ))

(princ "\n[ETS13] make_multicontour_etalons загружен. Команда: ETS13.")
(princ)
