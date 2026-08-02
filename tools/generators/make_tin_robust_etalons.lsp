;;; ETS16 — надёжность авто-TIN на вогнутом контуре с проёмом.
;;; Требует make_tin_etalons.lsp (общие безопасные примитивы ETS15).
;;;
;;; Команды:
;;;   ETS16             — основной сложный сценарий, 16 отметок.
;;;   ETS16MANUAL       — добавить одну ручную область внутрь контура.
;;;   ETS16CLEARMANUAL  — удалить только эту ручную область перед Var.
;;;   ETS16MISSING      — негативный сценарий: нет отметки в вершине контура.
;;;   ETS16CONFLICT     — негативный сценарий: две разные отметки в одной точке.

(vl-load-com)

(setq *ets16-boundary* nil)
(setq *ets16-manual* nil)

(setq *ets16-outer-points*
  '((0.0 0.0) (6000.0 0.0) (6000.0 2000.0)
    (3500.0 2000.0) (3500.0 6000.0) (0.0 6000.0)))

(setq *ets16-hole-points*
  '((1000.0 500.0) (2000.0 500.0)
    (2000.0 1500.0) (1000.0 1500.0)))

(setq *ets16-inner-points*
  '((3000.0 1000.0) (5000.0 1000.0)
    (1000.0 3500.0) (2500.0 3500.0)
    (1000.0 5000.0) (2500.0 5000.0)))

(defun ets16-height-text (point / value)
  ;; Все отметки лежат на одной плоскости H=(X+Y)/500.
  (setq value (/ (+ (car point) (cadr point)) 500.0))
  (if (equal value (float (fix value)) 1e-9)
    (itoa (fix value))
    (rtos value 2 3)))

(defun ets16-insert-points (points missing-p / point)
  (foreach point points
    (if (not (and missing-p (equal point '(0.0 0.0) 1e-8)))
      (ets15-insert-mark point (ets16-height-text point) 256))))

(defun ets16-build (mode / missing-p conflict-p all-points)
  (setq missing-p (= mode "missing"))
  (setq conflict-p (= mode "conflict"))
  (setq *ets16-boundary* nil *ets16-manual* nil)
  (ets11-ensure-support-table)
  (ets15-ensure-layer "контур" 7)
  (ets15-ensure-layer "областиvb" 1)
  (ets15-ensure-layer "линии_высот" 8)
  ;; Проём создаётся до внешнего контура; внешний контур остаётся верхним
  ;; объектом и легко выбирается после ETS16.
  (ets15-polyline *ets16-hole-points* "областиvb")
  (setq all-points
    (append *ets16-outer-points* *ets16-hole-points* *ets16-inner-points*))
  (ets16-insert-points all-points missing-p)
  (if conflict-p
    (ets15-insert-mark '(3000.0 1000.0) "99" 256))
  (setq *ets16-boundary* (ets15-polyline *ets16-outer-points* "контур"))
  (vla-ZoomExtents (vlax-get-acad-object))
  T)

(defun c:ETS16 ()
  (ets16-build "normal")
  (princ "\n[ETS16] Готово: вогнутый контур, один проём, 16 отметок на плоскости.")
  (princ "\n[ETS16] OPORTIN: ожидается отметок=16, raw=25, constrained TIN=22, вне области=3.")
  (princ "\n[ETS16] После TIN: TINDUMP -> TRI=22, XDATA_TIN=22, HOLES=1, BAD_CENTROIDS=0.")
  (princ))

(defun c:ETS16MANUAL ()
  (if (and *ets16-manual* (entget *ets16-manual*))
    (entdel *ets16-manual*))
  ;; Все три вершины уже имеют отметки. Область находится внутри верхней части
  ;; Г-образного контура и намеренно не получает XData OPOR.
  (setq *ets16-manual*
    (ets15-polyline
      '((0.0 6000.0) (1000.0 5000.0) (2500.0 5000.0))
      "линии_высот"))
  (princ "\n[ETS16MANUAL] Добавлена одна ручная область без XData.")
  (princ "\n[ETS16MANUAL] Повтори OPORTIN: должно стать TRI=23, XDATA_TIN=22, MANUAL_TRI=1.")
  (princ))

(defun c:ETS16CLEARMANUAL ()
  (if (and *ets16-manual* (entget *ets16-manual*))
    (progn
      (entdel *ets16-manual*)
      (setq *ets16-manual* nil)
      (princ "\n[ETS16CLEARMANUAL] Ручная контрольная область удалена."))
    (princ "\n[ETS16CLEARMANUAL] Контрольная ручная область не найдена."))
  (princ))

(defun c:ETS16MISSING ()
  (ets16-build "missing")
  (princ "\n[ETS16MISSING] Нет отметки в вершине 0,0.")
  (princ "\n[ETS16MISSING] OPORTIN должен отказать: не хватает отметок=1; TRI=0.")
  (princ))

(defun c:ETS16CONFLICT ()
  (ets16-build "conflict")
  (princ "\n[ETS16CONFLICT] В точке 3000,1000 стоят отметки 8 и 99.")
  (princ "\n[ETS16CONFLICT] OPORTIN должен отказать: конфликтов=1; TRI=0.")
  (princ))

(princ "\n[ETS16] make_tin_robust_etalons загружен. Команды: ETS16, ETS16MANUAL, ETS16CLEARMANUAL, ETS16MISSING, ETS16CONFLICT.")
(princ)
