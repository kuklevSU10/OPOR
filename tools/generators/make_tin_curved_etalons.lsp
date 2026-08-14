;;; ETS17 — округлые контуры: bulge-дуги внешней границы, вогнутая дуга и круглый проём.
;;; Требует make_tin_etalons.lsp (слои, блоки отметок и безопасные примитивы ETS15).
;;;
;;; Команды:
;;;   ETS17        — выпуклый капсульный контур и круглый проём для OPORTIN + Var.
;;;   ETS17CONCAVE — внешний контур с вогнутой дугой для OPORTIN.
;;;   ETS17VAR     — капсульный контур и такая же ручная область высот для прямого Var.

(vl-load-com)

(setq *ets17-boundary* nil)

(setq *ets17-capsule-data*
  '((0.0 0.0 0.0)
    (6000.0 0.0 1.0)
    (6000.0 4000.0 0.0)
    (0.0 4000.0 1.0)))

(setq *ets17-hole-data*
  '((2500.0 2000.0 1.0)
    (3500.0 2000.0 1.0)))

(setq *ets17-concave-data*
  '((12000.0 0.0 0.0)
    (18000.0 0.0 0.0)
    (18000.0 5000.0 -0.5)
    (12000.0 5000.0 0.0)))

(defun ets17-polyline (vertices layer / coords item arr pline index)
  (setq coords '())
  (foreach item vertices
    (setq coords (append coords (list (car item) (cadr item)))))
  (setq arr
    (vlax-make-safearray
      vlax-vbDouble (cons 0 (1- (length coords)))))
  (vlax-safearray-fill arr coords)
  (setq pline
    (vla-AddLightWeightPolyline (opor-ms) arr))
  (vla-put-Closed pline :vlax-true)
  (vla-put-Layer pline layer)
  (setq index 0)
  (foreach item vertices
    (if (not (equal (caddr item) 0.0 1e-12))
      (vla-SetBulge pline index (caddr item)))
    (setq index (1+ index)))
  (vlax-vla-object->ename pline))

(defun ets17-height-text (point / value)
  (setq value (/ (+ (car point) (* 2.0 (cadr point))) 1000.0))
  (if (equal value (float (fix value)) 1e-9)
    (itoa (fix value))
    (rtos value 2 3)))

(defun ets17-insert-points (points / point block created ok)
  (setq created '() ok T)
  (foreach point points
    (setq block (ets15-insert-mark point (ets17-height-text point) 256))
    (if block
      (setq created (cons block created))
      (setq ok nil)))
  (if (not ok)
    (progn
      (foreach block created
        (vl-catch-all-apply 'vla-Delete (list block)))
      (opor-alert "ETS17: не удалось создать все тестовые отметки высот.")))
  ok)

(defun ets17-ready ()
  (cond
    ((not (member "OPOR-IMPORT-LEVEL-BLOCK" (atoms-family 1)))
      (opor-alert "ETS17: сначала загрузите 00_APPLOAD/01_OPOR_PORT.lsp.")
      nil)
    ((not (opor-import-level-block))
      (opor-alert "ETS17: не удалось загрузить блок отметки из библиотеки OPOR.")
      nil)
    (t
      (ets15-ensure-layer "контур" 7)
      (ets15-ensure-layer "областиvb" 1)
      (ets15-ensure-layer "линии_высот" 8)
      T)))

(defun c:ETS17 ()
  (if (and
        (ets17-ready)
        (ets17-insert-points
          '((0.0 0.0) (6000.0 0.0) (6000.0 4000.0) (0.0 4000.0)
            (1500.0 2000.0) (4500.0 2000.0) (3000.0 500.0) (3000.0 3500.0))))
    (progn
      ;; Проём создаётся до внешнего контура; контур остаётся удобным для выбора.
      (ets17-polyline *ets17-hole-data* "областиvb")
      (setq *ets17-boundary* (ets17-polyline *ets17-capsule-data* "контур"))
      (vla-ZoomExtents (vlax-get-acad-object))
      (princ "\n[ETS17] Готово: выпуклый контур из двух полуокружностей и круглый проём.")
      (princ "\n[ETS17] OPORTIN -> выбрать контур -> TINDUMP: CURVE_SAMPLES>0, HOLES=1,")
      (princ "\n[ETS17] BAD_CENTROIDS=0, SLOPES IN_HOLES=0. Затем можно запускать Var.")))
  (princ))

(defun c:ETS17CONCAVE ()
  (if (and
        (ets17-ready)
        (ets17-insert-points
          '((12000.0 0.0) (18000.0 0.0) (18000.0 5000.0) (12000.0 5000.0)
            (13000.0 1000.0) (15000.0 2000.0) (17000.0 1000.0))))
    (progn
      (setq *ets17-boundary* (ets17-polyline *ets17-concave-data* "контур"))
      (vla-ZoomExtents (vlax-get-acad-object))
      (princ "\n[ETS17CONCAVE] Готово: верхняя граница — вогнутая bulge-дуга.")
      (princ "\n[ETS17CONCAVE] OPORTIN -> TINDUMP: CURVE_SAMPLES>0 и BAD_CENTROIDS=0.")))
  (princ))

(defun c:ETS17VAR ()
  (if (and
        (ets17-ready)
        (ets17-insert-points
          '((0.0 0.0) (6000.0 0.0) (6000.0 4000.0) (0.0 4000.0))))
    (progn
      ;; Ручная область высот повторяет внешний контур. Создаём её до контура,
      ;; чтобы последний объект под курсором был именно выбираемой границей.
      (ets17-polyline *ets17-capsule-data* "линии_высот")
      (setq *ets17-boundary* (ets17-polyline *ets17-capsule-data* "контур"))
      (vla-ZoomExtents (vlax-get-acad-object))
      (princ "\n[ETS17VAR] Готово: округлый контур и совпадающая дуговая область высот.")
      (princ "\n[ETS17VAR] Запусти B/Var без OPORTIN; ожидается 0 ошибок высот.")))
  (princ))

(princ "\n[ETS17] make_tin_curved_etalons загружен. Команды: ETS17, ETS17CONCAVE, ETS17VAR.")
(princ)
