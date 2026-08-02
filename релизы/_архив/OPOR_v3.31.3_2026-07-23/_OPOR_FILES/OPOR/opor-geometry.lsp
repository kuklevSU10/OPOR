;;; OPOR geometry and AutoCAD object helpers

(defun opor-to-vla (value)
  (cond
    ((= (type value) 'VLA-OBJECT) value)
    ((= (type value) 'ENAME) (vlax-ename->vla-object value))
    (t nil)))

(defun opor-obj-name (obj)
  (if obj (vla-get-ObjectName obj) ""))

(defun opor-polyline-object-p (obj / name)
  (setq name (opor-obj-name obj))
  (or (= name "AcDbPolyline")
      (= name "AcDb2dPolyline")
      (= name "AcDb3dPolyline")))

(defun opor-line-object-p (obj)
  (= (opor-obj-name obj) "AcDbLine"))

(defun opor-block-exists-p (name)
  (if (tblsearch "BLOCK" name) T nil))

(defun opor-layer-exists-p (name)
  (if (tblsearch "LAYER" name) T nil))

;; существующие слои НЕ перекрашиваем — цвет/тип только новым
(defun opor-ensure-layer (name color linetype / layers layer existed)
  (setq layers (vla-get-Layers (opor-doc)))
  (setq existed (opor-layer-exists-p name))
  (if existed
    (setq layer (vla-Item layers name))
    (progn
      (setq layer (vla-Add layers name))
      (if color (vl-catch-all-apply 'vla-put-Color (list layer color)))
      (if linetype (vl-catch-all-apply 'vla-put-Linetype (list layer linetype)))))
  (vl-catch-all-apply 'vla-put-Freeze (list layer :vlax-false))
  (vl-catch-all-apply 'vla-put-LayerOn (list layer :vlax-true))
  layer)

(defun opor-ensure-layers ()
  (foreach def *opor-layer-defs*
    (opor-ensure-layer (car def) (cadr def) (caddr def)))
  T)

(defun opor-v+ (a b)
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)) (+ (caddr a) (caddr b))))

(defun opor-v- (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr a) (caddr b))))

(defun opor-v* (a s)
  (list (* (car a) s) (* (cadr a) s) (* (caddr a) s)))

(defun opor-dot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)) (* (caddr a) (caddr b))))

(defun opor-dist (a b)
  (distance a b))

(defun opor-unit (a / d)
  (setq d (distance '(0.0 0.0 0.0) a))
  (if (equal d 0.0 1e-12)
    nil
    (opor-v* a (/ 1.0 d))))

(defun opor-perp2d (v)
  (list (- (cadr v)) (car v) 0.0))

(defun opor-2d (pt)
  (list (car pt) (cadr pt) 0.0))

(defun opor-safearray->points (arr / lst res)
  (setq res '())
  (if (and (= (type arr) 'safearray)
           (>= (vlax-safearray-get-u-bound arr 1)
               (vlax-safearray-get-l-bound arr 1)))
    (progn
      (setq lst (vlax-safearray->list arr))
      (while (and lst (cddr lst))
        (setq res (cons (list (car lst) (cadr lst) (caddr lst)) res))
        (setq lst (cdddr lst)))))
  (reverse res))

(defun opor-obj-intersections (o1 o2 / result)
  (if (not (boundp 'acExtendNone)) (setq acExtendNone 0))
  (setq result
    (vl-catch-all-apply
      '(lambda ()
         (opor-safearray->points
           (vlax-variant-value (vla-IntersectWith o1 o2 acExtendNone))))))
  (if (vl-catch-all-error-p result) '() result))

(defun opor-bbox (obj / ll ur r)
  (setq r
    (vl-catch-all-apply
      '(lambda ()
         (vla-GetBoundingBox obj 'll 'ur)
         (list (vlax-safearray->list ll) (vlax-safearray->list ur)))))
  (if (vl-catch-all-error-p r) nil r))

(defun opor-bbox-diagonal (bbox)
  (if bbox (distance (car bbox) (cadr bbox)) 0.0))

(defun opor-curve-start (obj)
  (vlax-curve-getStartPoint obj))

(defun opor-curve-end (obj)
  (vlax-curve-getEndPoint obj))

(defun opor-polyline-closed-p (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-Closed (list obj)))
  (if (vl-catch-all-error-p value) nil (equal value :vlax-true)))

(defun opor-polyline-points (obj / start end idx pts)
  (setq pts '())
  (setq start (fix (vlax-curve-getStartParam obj)))
  (setq end (fix (vlax-curve-getEndParam obj)))
  (setq idx start)
  (while (<= idx end)
    (setq pts (cons (vlax-curve-getPointAtParam obj idx) pts))
    (setq idx (1+ idx)))
  (reverse pts))

;; Вершины как в VBA pl.Coordinates: у замкнутой полилинии
;; без повтора стартовой точки в конце.
(defun opor-polyline-vertices (obj / pts last first)
  (setq pts (opor-polyline-points obj))
  (if (and (cdr pts) (opor-polyline-closed-p obj))
    (progn
      (setq first (car pts))
      (setq last (opor-list-tail pts))
      (if (< (distance (opor-2d first) (opor-2d last)) 1e-8)
        (setq pts (reverse (cdr (reverse pts)))))))
  pts)

(defun opor-make-temp-ray (pt / far ray)
  (setq far (list (+ (car pt) 1.0e7) (+ (cadr pt) 3.71e6) 0.0))
  (setq ray (vla-AddLine (opor-ms) (vlax-3d-point pt) (vlax-3d-point far)))
  ray)

(defun opor-point-inside-boundary-p (pt boundary / ray count)
  (setq ray (opor-make-temp-ray (opor-2d pt)))
  (setq count (length (opor-obj-intersections ray boundary)))
  (vl-catch-all-apply 'vla-Delete (list ray))
  (= 1 (rem count 2)))

(defun opor-point-in-holes-p (pt holes)
  (vl-some '(lambda (hole) (opor-point-inside-boundary-p pt hole)) holes))

(defun opor-point-on-curve-p (pt curve tol / closest)
  (setq closest
    (vl-catch-all-apply
      '(lambda () (vlax-curve-getClosestPointTo curve (opor-2d pt)))))
  (if (vl-catch-all-error-p closest)
    nil
    (<= (distance (opor-2d pt) (opor-2d closest)) tol)))

(defun opor-point-in-working-area-p (pt boundary holes)
  (and
    (or
      (opor-point-inside-boundary-p pt boundary)
      (opor-point-on-curve-p pt boundary *opor-point-tolerance*))
    (not
      (vl-some
        '(lambda (hole)
           (and
             (opor-point-inside-boundary-p pt hole)
             (not (opor-point-on-curve-p pt hole *opor-point-tolerance*))))
        holes))))

(defun opor-region-probe (obj / raw regions region message)
  (setq raw
    (vl-catch-all-apply
      'vlax-invoke
      (list (opor-ms) 'AddRegion (list obj))))
  (if (vl-catch-all-error-p raw)
    (list nil (vl-catch-all-error-message raw))
    (progn
      (setq regions
        (cond
          ((= (type raw) 'LIST) raw)
          ((= (type raw) 'VLA-OBJECT) (list raw))
          (t (opor-variant-list raw))))
      ;; chk_reg удаляет временный регион; порт чистит все элементы массива,
      ;; чтобы при неожиданном множественном результате ничего не оставить.
      (foreach region regions
        (vl-catch-all-apply 'vla-Delete (list region)))
      (if regions
        (list T nil)
        (list nil "AddRegion не вернул регион.")))))

(defun opor-region-invalid-input-p (message / upper)
  (setq upper (strcase (opor-string message)))
  (or
    (wcmatch upper "*INVALID INPUT*")
    (wcmatch upper "*НЕВЕРН*ВВОД*")
    (wcmatch upper "*НЕДОПУСТИМ*")))

(defun opor-polyline-region-valid-p (obj object-label / result message)
  (setq result (opor-region-probe obj))
  (if (car result)
    T
    (progn
      (setq message (cadr result))
      (if (opor-region-invalid-input-p message)
        (progn
          (opor-log (strcat "chk_reg: отклонён " object-label " — самопересечение."))
          (opor-alert
            (strcat
              object-label " выполнен из полилинии с самопересечением."
              "\n\nНеобходимо перерисовать " object-label ".")))
        (opor-alert
          (strcat
            "Ошибка проверки " object-label ".\n"
            (opor-string message))))
      nil)))

(defun opor-select-outer-boundary (/ picked obj answer candidate)
  (setq picked
    (entsel
      (if (> (if (opor-session-get 'contour-count) (opor-session-get 'contour-count) 0) 0)
        "\nВыберите следующий внешний контур OPOR или Enter для завершения: "
        "\nВыберите внешний контур OPOR: ")))
  (if picked
    (progn
      (setq obj (vlax-ename->vla-object (car picked)))
      (setq candidate
        (cond
          ((not (opor-polyline-object-p obj))
            (opor-alert "Выбранный объект не является полилинией.")
            nil)
          ((not (opor-polyline-closed-p obj))
            ;; В VBA chk_reg стоит до этого вопроса. Порт сначала штатно
            ;; замыкает контур, затем проверяет уже пригодную для Region кривую.
            (initget "Да Нет")
            (setq answer (getkword "\nПолилиния не замкнута. Замкнуть? [Да/Нет] <Да>: "))
            (if (or (not answer) (= answer "Да"))
              (progn
                (vl-catch-all-apply 'vla-put-Closed (list obj :vlax-true))
                (if (opor-polyline-closed-p obj)
                  obj
                  (progn
                    (opor-alert "Не удалось замкнуть полилинию.")
                    nil)))
              nil))
          (t obj)))
      (if (and candidate (opor-polyline-region-valid-p candidate "Контур"))
        candidate
        nil))
    nil))

;; ТЗ П9: площадь спецификации = контур минус проёмы (областиvb)
(defun opor-net-area (boundary holes / area value)
  (setq area (vla-get-Area boundary))
  (foreach hole holes
    (setq value (vl-catch-all-apply 'vla-get-Area (list hole)))
    (if (not (vl-catch-all-error-p value))
      (setq area (- area value))))
  area)

(defun opor-select-holes (/ ss idx holes obj)
  (setq holes '())
  (princ "\nВыберите проёмы на слое областиvb или нажмите Enter, если проёмов нет: ")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 8 *opor-layer-holes*))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss idx)))
        (if (and (opor-polyline-object-p obj) (opor-polyline-closed-p obj))
          (setq holes (cons obj holes)))
        (setq idx (1+ idx)))))
  (reverse holes))

;; UX v3.7: проёмы не выбираются руками — все замкнутые полилинии на областиvb,
;; все вершины которых внутри контура или на нём
(defun opor-detect-holes (boundary / holes obj inside)
  (setq holes '())
  (vlax-for obj (opor-ms)
    (if (and
          (= (vla-get-ObjectName obj) "AcDbPolyline")
          (= (strcase (vla-get-Layer obj)) (strcase *opor-layer-holes*))
          (opor-polyline-closed-p obj)
          (not (equal obj boundary)))
      (progn
        (setq inside T)
        (foreach pt (opor-polyline-vertices obj)
          (if (and inside
                   (not (opor-point-inside-boundary-p (opor-2d pt) boundary))
                   (not (opor-point-on-curve-p (opor-2d pt) boundary *opor-point-tolerance*)))
            (setq inside nil)))
        (if inside (setq holes (cons obj holes))))))
  (setq holes (reverse holes))
  (opor-log (strcat "Проёмов найдено: " (itoa (length holes))))
  holes)

(defun opor-hole-regions-valid-p (holes / valid index hole)
  (setq valid T index 1)
  (foreach hole holes
    (if valid
      (if (not
            (opor-polyline-region-valid-p
              hole (strcat "Проём " (itoa index))))
        (setq valid nil)))
    (setq index (1+ index)))
  valid)

(defun opor-command-region-check (/ boundary)
  (setq boundary (opor-select-outer-boundary))
  (if boundary
    (opor-log "chk_reg: полилиния пригодна для создания Region."))
  (if boundary T nil))

;; UX v3.7: центрирование экрана на контуре (+10% отступ)
(defun opor-zoom-to-boundary (boundary / bbox ll ur dx dy)
  (setq bbox (opor-bbox boundary))
  (if bbox
    (progn
      (setq ll (car bbox))
      (setq ur (cadr bbox))
      (setq dx (* 0.1 (max 1.0 (- (car ur) (car ll)))))
      (setq dy (* 0.1 (max 1.0 (- (cadr ur) (cadr ll)))))
      (vl-catch-all-apply 'vla-ZoomWindow
        (list (vlax-get-acad-object)
              (vlax-3d-point (list (- (car ll) dx) (- (cadr ll) dy) 0.0))
              (vlax-3d-point (list (+ (car ur) dx) (+ (cadr ur) dy) 0.0)))))))

(princ)
