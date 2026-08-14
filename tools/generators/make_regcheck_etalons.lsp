;;; ETS12 — chk_reg: валидный контур, самопересекающийся контур и плохой проём.
;;; ETS18 — защита от пересекающихся и вложенных проёмов.

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

(defun ets18-dxf-points (data / points item)
  (setq points '())
  (foreach item data
    (if (= (car item) 10)
      (setq points (cons (cdr item) points))))
  (reverse points))

(defun ets18-points-equal-p (a b / equal-p)
  (setq equal-p (= (length a) (length b)))
  (while (and equal-p a b)
    (if (not (equal (car a) (car b) 1e-6))
      (setq equal-p nil))
    (setq a (cdr a) b (cdr b)))
  equal-p)

;; Первая live-версия ETS18 сохраняла результат entmake (DXF-список), а не
;; ename. Сопоставление по слою и точным вершинам позволяет безопасно убрать
;; уже созданный тест после перезагрузки исправленного генератора.
(defun ets18-find-legacy-entity (snapshot / ss index entity data found)
  (setq ss
    (ssget "_X"
      (list
        (cons 0 (cdr (assoc 0 snapshot)))
        (cons 8 (cdr (assoc 8 snapshot))))))
  (setq index 0 found nil)
  (while (and ss (< index (sslength ss)) (not found))
    (setq entity (ssname ss index))
    (setq data (entget entity))
    (if (and
          (= (cdr (assoc 90 data)) (cdr (assoc 90 snapshot)))
          (= (cdr (assoc 70 data)) (cdr (assoc 70 snapshot)))
          (ets18-points-equal-p
            (ets18-dxf-points data)
            (ets18-dxf-points snapshot)))
      (setq found entity))
    (setq index (1+ index)))
  found)

(defun ets18-delete-entities (/ entity target deleted)
  (setq deleted 0)
  (if (and (boundp '*ets18-entities*) *ets18-entities*)
    (foreach entity *ets18-entities*
      (setq target
        (cond
          ((and (= (type entity) 'ENAME) (entget entity)) entity)
          ((= (type entity) 'LIST) (ets18-find-legacy-entity entity))
          (T nil)))
      (if (and target (entget target) (entdel target))
        (setq deleted (1+ deleted)))))
  (setq *ets18-entities* nil)
  deleted)

(defun ets18-add-rect (base x1 y1 x2 y2 layer / result entity)
  (setq result
    (ets12-rect
      (+ (car base) x1)
      (+ (cadr base) y1)
      (+ (car base) x2)
      (+ (cadr base) y2)
      layer))
  (if result
    (progn
      (setq entity (entlast))
      (setq *ets18-entities* (cons entity *ets18-entities*))))
  entity)

(defun c:ETS18 (/ base)
  (ets12-ensure-layer "контур" 7)
  (ets12-ensure-layer "областиvb" 1)
  (ets18-delete-entities)
  (setq base (getpoint "\nУкажите место для теста ETS18: "))
  (if base
    (progn
      ;; Общий внешний контур 12 x 7 м.
      (ets18-add-rect base 0.0 0.0 12000.0 7000.0 "контур")
      ;; Слева — два проёма, границы которых пересекаются в двух точках.
      (ets18-add-rect base 1000.0 1000.0 4000.0 3500.0 "областиvb")
      (ets18-add-rect base 2500.0 2000.0 5000.0 4500.0 "областиvb")
      ;; Справа — один проём полностью вложен в другой.
      (ets18-add-rect base 7000.0 1000.0 11000.0 5500.0 "областиvb")
      (ets18-add-rect base 8000.0 2000.0 9500.0 3500.0 "областиvb")
      (vla-ZoomExtents (vlax-get-acad-object))
      (princ "\n[ETS18] Создано: внешний контур и четыре проёма.")
      (princ "\n[ETS18] Слева проёмы пересекаются, справа один вложен в другой.")
      (princ "\n[ETS18] Запустите OPORTIN и выберите созданный внешний контур.")
      (princ "\n[ETS18] Ожидается: проблемных пар=2, оранжевых маркеров=3, TIN не строится."))
    (princ "\n[ETS18] Отменено."))
  (princ))

(defun c:ETS18CLEAN (/ deleted)
  (setq deleted (ets18-delete-entities))
  (princ
    (strcat
      "\n[ETS18] Удалено тестовых контуров и проёмов: "
      (itoa deleted) "."))
  (princ))

(princ "\n[ETS12/ETS18] make_regcheck_etalons загружен. Команды: ETS12, ETS18, ETS18CLEAN.")
(princ)
