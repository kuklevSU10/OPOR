;;; ETS10 — эталон +0.000/auto_levl.

(vl-load-com)

(defun ets10-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name) (cons 70 0) (cons 62 color) (cons 6 "Continuous")))))

(defun ets10-rect (x1 y1 x2 y2 layer)
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDbPolyline")
      (cons 90 4) (cons 70 1)
      (list 10 x1 y1) (list 10 x2 y1)
      (list 10 x2 y2) (list 10 x1 y2))))

(defun c:ETS10 ()
  (if (not (tblsearch "BLOCK" "otmetka_oporvb"))
    (princ "\n[ETS10] ОШИБКА: нет определения блока otmetka_oporvb.")
    (progn
      (ets10-ensure-layer "контур" 7)
      (ets10-ensure-layer "линии_высот" 8)
      (setvar "CLAYER" "0")
      (ets10-rect -500.0 -500.0 3500.0 2500.0 "контур")
      ;; Две соседние области: 8 вершин, две пары совпадают → 6 уникальных.
      (ets10-rect 0.0 0.0 1000.0 1000.0 "линии_высот")
      (ets10-rect 1000.0 0.0 2000.0 1000.0 "линии_высот")
      ;; Crossing bbox цепляет область частично снаружи и добавляет все 4 вершины.
      (ets10-rect 3000.0 2000.0 4000.0 3000.0 "линии_высот")
      (vla-ZoomExtents (vlax-get-acad-object))
      (princ "\n[ETS10] Готово: выбери внешний контур; ожидается 10 отметок.")))
  (princ))

(princ "\n[ETS10] make_auto_level_etalons загружен. Команда: ETS10.")
(princ)
