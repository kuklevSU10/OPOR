;;; make_etalons.lsp — генератор эталонной геометрии OPOR (2026-07-09).
;;; Запускать в НОВОМ чертеже из шаблона "исходник/LEVEL 2025 V3+.dwt".
;;; Порядок (один чертёж, инкрементально):
;;;   ETS1 -> _SAVEAS эталоны/S1_lisp.dwg
;;;   ETS3 -> _SAVEAS эталоны/S3_lisp.dwg, затем ещё раз как S5_lisp.dwg
;;;   ETS2 -> _SAVEAS эталоны/s2_lisp.dwg, затем как "S5_lisp PRO.dwg"
;;; Геометрия по эталоны/README.md:
;;;   S1: контур 3000x5000 в (0,0), слой "контур".
;;;   S3: + проём 900x1200, угол (1000,1500), слой "областиvb".
;;;   S2: + область высот по углам контура на "линии_высот",
;;;       блоки otmetka_oporvb в вершинах: (0,0)=0 (3000,0)=10
;;;       (3000,5000)=20 (0,5000)=10.
;;; Кодировка файла: CP1251.

(vl-load-com)

(defun ets-ensure-layer (name color)
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

(defun ets-rect (x1 y1 x2 y2 layer)
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

(defun ets-insert-mark (x y val / doc ms blk atts)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq blk
    (vl-catch-all-apply
      'vla-InsertBlock
      (list ms (vlax-3d-point (list x y 0.0)) "otmetka_oporvb" 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p blk)
    (progn
      (princ
        (strcat "\n[ETS] ОШИБКА вставки otmetka_oporvb: "
                (vl-catch-all-error-message blk)
                "\n[ETS] Блока нет в шаблоне? Проверь DWT."))
      nil)
    (progn
      (vla-put-Layer blk "линии_высот")
      (setq atts (vl-catch-all-apply 'vlax-invoke (list blk 'GetAttributes)))
      (if (and (not (vl-catch-all-error-p atts)) atts)
        (vla-put-TextString (car atts) val)
        (princ "\n[ETS] ВНИМАНИЕ: у блока нет атрибутов — значение отметки не записано!"))
      blk)))

(defun c:ETS1 ()
  (ets-ensure-layer "контур" 7)
  (ets-rect 0.0 0.0 3000.0 5000.0 "контур")
  (princ "\n[ETS] S1 готов: контур 3000x5000 в (0,0) на слое 'контур'.")
  (princ "\n[ETS] Сохрани: _SAVEAS -> эталоны/S1_lisp.dwg. Дальше ETS3.")
  (princ))

(defun c:ETS3 ()
  (ets-ensure-layer "областиvb" 1)
  (ets-rect 1000.0 1500.0 1900.0 2700.0 "областиvb")
  (princ "\n[ETS] S3 готов: проём 900x1200 (углы 1000,1500 - 1900,2700) на 'областиvb'.")
  (princ "\n[ETS] Сохрани: _SAVEAS -> S3_lisp.dwg, потом ещё раз -> S5_lisp.dwg. Дальше ETS2.")
  (princ))

(defun c:ETS2 ()
  (ets-ensure-layer "линии_высот" 8)
  (ets-rect 0.0 0.0 3000.0 5000.0 "линии_высот")
  (foreach m '((0.0 0.0 "0") (3000.0 0.0 "10") (3000.0 5000.0 "20") (0.0 5000.0 "10"))
    (ets-insert-mark (car m) (cadr m) (caddr m)))
  (princ "\n[ETS] S2 готов: область высот по углам + отметки 0/10/20/10.")
  (princ "\n[ETS] Сохрани: _SAVEAS -> s2_lisp.dwg, потом ещё раз -> 'S5_lisp PRO.dwg'. Всё.")
  (princ))

(princ "\n[ETS] make_etalons загружен. Команды: ETS1 (контур), ETS3 (+проём), ETS2 (+высоты). Порядок: ETS1 -> ETS3 -> ETS2, с _SAVEAS между шагами.")
(princ)
