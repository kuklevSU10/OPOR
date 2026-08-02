;;; ETS12 — chk_reg: валидный контур, самопересекающийся контур и плохой проём.

(vl-load-com)

(defun ets12-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name) (cons 70 0) (cons 62 color) (cons 6 "Continuous")))))

(defun ets12-polyline (points layer / data)
  (setq data
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDbPolyline")
      (cons 90 (length points))
      (cons 70 1)))
  (foreach pt points
    (setq data (append data (list (list 10 (car pt) (cadr pt))))))
  (entmake data))

(defun ets12-rect (x1 y1 x2 y2 layer)
  (ets12-polyline
    (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2))
    layer))

(defun ets12-bowtie (x1 y1 x2 y2 layer)
  (ets12-polyline
    (list (list x1 y1) (list x2 y2) (list x1 y2) (list x2 y1))
    layer))

(defun c:ETS12 ()
  (ets12-ensure-layer "контур" 7)
  (ets12-ensure-layer "областиvb" 1)
  ;; Слева — заведомо валидная полилиния.
  (ets12-rect 0.0 0.0 2000.0 2000.0 "контур")
  ;; По центру — самопересекающаяся «бабочка».
  (ets12-bowtie 3000.0 0.0 5000.0 2000.0 "контур")
  ;; Справа — валидный внешний контур с самопересекающимся проёмом.
  (ets12-rect 6000.0 0.0 10000.0 3000.0 "контур")
  (ets12-bowtie 7000.0 500.0 9000.0 2500.0 "областиvb")
  (vla-ZoomExtents (vlax-get-acad-object))
  (princ "\n[ETS12] Слева: валидный прямоугольник; центр: плохой контур;")
  (princ "\n[ETS12] справа: валидный контур с плохим проёмом.")
  (princ "\n[ETS12] Проверка: OPORREGCHECK слева=OK, центр=самопересечение;")
  (princ "\n[ETS12] OPOR Const справа должен остановиться на проёме до формы параметров.")
  (princ))

(princ "\n[ETS12] make_regcheck_etalons загружен. Команда: ETS12.")
(princ)
