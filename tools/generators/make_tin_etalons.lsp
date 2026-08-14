;;; ETS15 — компактный эталон автоматической TIN-триангуляции.
;;; Команды:
;;;   ETS15       — контур 3000x5000 и пять отметок (четыре угла + центр).
;;;   ETS15MANUAL — четыре ручных треугольника для независимого сравнения с OPORTIN.

(vl-load-com)

(setq *ets15-boundary* nil)

(defun ets15-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name) (cons 70 0) (cons 62 color) (cons 6 "Continuous")))))

(defun ets15-polyline (points layer / data pt)
  (setq data
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity") (cons 8 layer)
      (cons 100 "AcDbPolyline") (cons 90 (length points)) (cons 70 1)))
  (foreach pt points
    (setq data (append data (list (list 10 (car pt) (cadr pt))))))
  ;; entmakex возвращает ename; он нужен ETS15MANUAL для удаления и
  ;; пересоздания внешнего контура поверх ручных треугольников.
  (entmakex data))

(defun ets15-variant-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (if (= (type value) 'safearray) (vlax-safearray->list value) '()))

(defun ets15-insert-mark (point text color / doc ms value block raw atts)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list ms (vlax-3d-point (append point '(0.0)))
            "otmetka_oporvb" 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq block value)
      (vla-put-Color block color)
      (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
      (if (not (vl-catch-all-error-p raw))
        (progn
          (setq atts (ets15-variant-list raw))
          (if atts (vla-put-TextString (car atts) text))))
      block)))

(defun ets15-create-boundary ()
  (setq *ets15-boundary*
    (ets15-polyline
      '((0.0 0.0) (3000.0 0.0) (3000.0 5000.0) (0.0 5000.0))
      "контур")))

(defun c:ETS15 ()
  ;; Старый ETS15 рассчитывал на готовый DWT. Для чистого DWG сначала явно
  ;; импортируем рабочее определение отметки из переносимой библиотеки OPOR.
  (if (not (opor-import-level-block))
    (princ "\n[ETS15] ОШИБКА: не удалось загрузить блок otmetka_oporvb из библиотеки OPOR.")
    (progn
      (ets15-ensure-layer "контур" 7)
      (ets15-ensure-layer "линии_высот" 8)
      (ets15-insert-mark '(0.0 0.0) "0" 256)
      (ets15-insert-mark '(3000.0 0.0) "10" 256)
      (ets15-insert-mark '(3000.0 5000.0) "20" 256)
      (ets15-insert-mark '(0.0 5000.0) "10" 256)
      ;; Красный центральный блок специально оставлен как проверка: drain-блок
      ;; не должен менять геометрию треугольных областей.
      (ets15-insert-mark '(1500.0 2500.0) "10" 1)
      ;; Контур создаётся последним, чтобы его было удобно выбрать до построения TIN.
      (ets15-create-boundary)
      (vl-catch-all-apply 'vla-ZoomExtents (list (vlax-get-acad-object)))
      (princ "\n[ETS15] Готово: контур 3000x5000, 5 отметок; центральная отметка красная.")
      (princ "\n[ETS15] Авто: OPORTIN -> выбрать внешний контур -> TINDUMP; ожидается 4 треугольника.")
      (princ "\n[ETS15] Затем Var: lev, 600/600, R100, доска, floor=101, доска=27, лага=49, сдвиг=0, vect.")))
  (princ))

(defun c:ETS15MANUAL ()
  ;; Удаляем и пересоздаём внешний контур последним, чтобы ручные линии не
  ;; перехватывали выбор контура в Var.
  (if (and *ets15-boundary* (entget *ets15-boundary*))
    (entdel *ets15-boundary*))
  (ets15-polyline '((0.0 0.0) (3000.0 0.0) (1500.0 2500.0)) "линии_высот")
  (ets15-polyline '((3000.0 0.0) (3000.0 5000.0) (1500.0 2500.0)) "линии_высот")
  (ets15-polyline '((3000.0 5000.0) (0.0 5000.0) (1500.0 2500.0)) "линии_высот")
  (ets15-polyline '((0.0 5000.0) (0.0 0.0) (1500.0 2500.0)) "линии_высот")
  (ets15-create-boundary)
  (vl-catch-all-apply 'vla-ZoomExtents (list (vlax-get-acad-object)))
  (princ "\n[ETS15MANUAL] Готово: построены 4 независимых ручных треугольника.")
  (princ "\n[ETS15MANUAL] TINDUMP должен показать TRI=4, XDATA_TIN=0.")
  (princ))

(princ "\n[ETS15] make_tin_etalons загружен. Команды: ETS15, ETS15MANUAL.")
(princ)
