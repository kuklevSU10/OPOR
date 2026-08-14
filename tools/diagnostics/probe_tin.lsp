;;; TINDUMP — независимая диагностика TIN и распределения высот опор.

(vl-load-com)

(defun tindump-variant-list (value)
  (if (= (type value) 'variant) (setq value (vlax-variant-value value)))
  (cond
    ((= (type value) 'safearray) (vlax-safearray->list value))
    ((= (type value) 'LIST) value)
    (t '())))

(defun tindump-round (value)
  (fix (+ value (if (< value 0.0) -0.5 0.5))))

(defun tindump-point-key (pt)
  (strcat (itoa (tindump-round (car pt))) "," (itoa (tindump-round (cadr pt)))))

(defun tindump-insert-sorted (value values)
  (cond
    ((not values) (list value))
    ((tindump-string-less-p value (car values)) (cons value values))
    (t (cons (car values) (tindump-insert-sorted value (cdr values))))))

(defun tindump-string-less-p (a b / sorted)
  (setq sorted (acad_strlsort (list a b)))
  (and (/= a b) (= a (car sorted))))

(defun tindump-polyline-points (obj / raw values result)
  (setq raw (vl-catch-all-apply 'vlax-get (list obj 'Coordinates)))
  (if (vl-catch-all-error-p raw)
    '()
    (progn
      (setq values (tindump-variant-list raw) result '())
      (while (>= (length values) 2)
        (setq result (cons (list (car values) (cadr values)) result))
        (setq values (cddr values)))
      (reverse result))))

(defun tindump-triangle-signature (points / keys pt)
  (setq keys '())
  (foreach pt points
    (setq keys (tindump-insert-sorted (tindump-point-key pt) keys)))
  (strcat (car keys) "|" (cadr keys) "|" (caddr keys)))

(defun tindump-xdata-type-p (obj object-type / en data app item found)
  (setq en (vlax-vla-object->ename obj))
  (if en
    (progn
      (setq data (entget en '("OPOR")))
      (setq app (cadr (assoc -3 data)) found nil)
      (if (and app (= (car app) "OPOR"))
        (foreach item (cdr app)
          (if (and (= (car item) 1000) (= (cdr item) object-type))
            (setq found T))))
      found)
    nil))

(defun tindump-xdata-tin-p (obj)
  (tindump-xdata-type-p obj "tin-triangle"))

(defun tindump-effective-name (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p value)
    (progn
      (setq value (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p value) "" value))
    value))

(defun tindump-inc (key values / pair)
  (if (setq pair (assoc key values))
    (subst (cons key (1+ (cdr pair))) pair values)
    (cons (cons key 1) values)))

(defun tindump-first-attribute (obj / raw atts value)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list obj)))
  (if (vl-catch-all-error-p raw)
    "?"
    (progn
      (setq atts (tindump-variant-list raw))
      (if atts
        (progn
          (setq value (vl-catch-all-apply 'vla-get-TextString (list (car atts))))
          (if (vl-catch-all-error-p value) "?" value))
        "?"))))

(defun tindump-pair-less-p (a b)
  (tindump-string-less-p (car a) (car b)))

(defun tindump-duplicate-count (values / previous count value)
  (setq previous nil count 0)
  (foreach value values
    (if (and previous (= value previous))
      (setq count (1+ count)))
    (setq previous value))
  count)

(defun tindump-object-handle (obj / value)
  (setq value (vl-catch-all-apply 'vla-get-Handle (list obj)))
  (if (vl-catch-all-error-p value) "?" value))

(defun tindump-point-near-any-p (pt points tol / found other)
  (setq found nil)
  (foreach other points
    (if (and (not found) (<= (distance pt other) tol))
      (setq found T)))
  found)

(defun tindump-point-on-segment-p (pt a b tol / dx dy len2 u qx qy)
  (setq dx (- (car b) (car a)) dy (- (cadr b) (cadr a)))
  (setq len2 (+ (* dx dx) (* dy dy)))
  (if (< len2 1e-12)
    (<= (distance pt a) tol)
    (progn
      (setq u
        (/ (+ (* (- (car pt) (car a)) dx)
              (* (- (cadr pt) (cadr a)) dy))
           len2))
      (if (or (< u 0.0) (> u 1.0))
        nil
        (progn
          (setq qx (+ (car a) (* u dx)) qy (+ (cadr a) (* u dy)))
          (<= (distance pt (list qx qy)) tol))))))

(defun tindump-point-on-polygon-p (pt points tol / prev item found)
  (setq found nil)
  (if points
    (progn
      (setq prev (car (reverse points)))
      (foreach item points
        (if (and (not found) (tindump-point-on-segment-p pt prev item tol))
          (setq found T))
        (setq prev item))))
  found)

(defun tindump-point-in-polygon-p (pt points / inside prev item xi yi xj yj)
  (setq inside nil)
  (if points
    (progn
      (setq prev (car (reverse points)))
      (foreach item points
        (setq xi (car item) yi (cadr item) xj (car prev) yj (cadr prev))
        (if (and (not (eq (> yi (cadr pt)) (> yj (cadr pt))))
                 (< (car pt)
                    (+ xi
                       (/ (* (- xj xi) (- (cadr pt) yi))
                          (- yj yi)))))
          (setq inside (not inside)))
        (setq prev item))))
  inside)

(defun tindump-point-in-any-polygon-p (pt polygons / found points)
  (setq found nil)
  (foreach points polygons
    (if (and (not found)
             (or (tindump-point-in-polygon-p pt points)
                 (tindump-point-on-polygon-p pt points 0.01)))
      (setq found T)))
  found)

(defun tindump-point-strictly-in-any-polygon-p (pt polygons / found points)
  (setq found nil)
  (foreach points polygons
    (if (and (not found)
             (tindump-point-in-polygon-p pt points)
             (not (tindump-point-on-polygon-p pt points 0.01)))
      (setq found T)))
  found)

(defun tindump-point-in-any-curve-p (pt curves / found curve)
  (setq found nil)
  (foreach curve curves
    (if (and (not found)
             (or (opor-point-inside-boundary-p pt curve)
                 (opor-point-on-curve-p pt curve *opor-point-tolerance*)))
      (setq found T)))
  found)

(defun tindump-point-strictly-in-any-curve-p (pt curves / found curve)
  (setq found nil)
  (foreach curve curves
    (if (and (not found)
             (opor-point-inside-boundary-p pt curve)
             (not (opor-point-on-curve-p pt curve *opor-point-tolerance*)))
      (setq found T)))
  found)

(defun tindump-centroid (points)
  (list
    (/ (+ (car (car points)) (car (cadr points)) (car (caddr points))) 3.0)
    (/ (+ (cadr (car points)) (cadr (cadr points)) (cadr (caddr points))) 3.0)))

(defun c:TINDUMP (/ doc ms obj layer points point triangles xdata signatures vertices signature key supports pair
                    contours holes centroid bad-centroids support-point supports-in-holes raw
                    block-point slopes auto-slopes slopes-in-holes slope-values curve-samples
                    auto-hole-marks auto-curve-marks is-auto-slope auto-slope-points
                    extra-slope-records extra-record extra-overlaps duplicate-triangles)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq contours '() holes '())
  ;; Первый проход: независимая геометрия рабочей области для QC центроидов
  ;; и запрета опор внутри проёмов.
  (vlax-for obj ms
    (setq layer (vl-catch-all-apply 'vla-get-Layer (list obj)))
    (if (not (vl-catch-all-error-p layer))
      (cond
        ((= (strcase layer) (strcase "контур"))
          (setq contours (cons obj contours)))
        ((= (strcase layer) (strcase "областиvb"))
          (setq holes (cons obj holes))))))
  (setq triangles 0 xdata 0 signatures '() vertices '() supports '()
        bad-centroids 0 supports-in-holes 0 slopes 0 auto-slopes 0
        slopes-in-holes 0 slope-values '() auto-hole-marks 0 auto-curve-marks 0
        auto-slope-points '() extra-slope-records '())
  (vlax-for obj ms
    (setq layer (vl-catch-all-apply 'vla-get-Layer (list obj)))
    (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
      (cond
        ((tindump-xdata-type-p obj "tin-interpolated-mark")
          (setq auto-hole-marks (1+ auto-hole-marks)))
        ((tindump-xdata-type-p obj "tin-interpolated-curve-mark")
          (setq auto-curve-marks (1+ auto-curve-marks)))))
    ;; VBA/COM может сообщать разные ObjectName для одинаковых 2D-полилиний.
    ;; Для диагностики достаточно слоя и ровно трёх координатных вершин.
    (if (and (not (vl-catch-all-error-p layer))
             (= (strcase layer) (strcase "линии_высот")))
      (progn
        (setq points (tindump-polyline-points obj))
        (if (= (length points) 3)
          (progn
            (setq triangles (1+ triangles))
            (if (tindump-xdata-tin-p obj) (setq xdata (1+ xdata)))
            (setq centroid (tindump-centroid points))
            (if (and contours
                     (or (not (tindump-point-in-any-curve-p centroid contours))
                         (tindump-point-strictly-in-any-curve-p centroid holes)))
              (setq bad-centroids (1+ bad-centroids)))
            (setq signature (tindump-triangle-signature points))
            (setq signatures (tindump-insert-sorted signature signatures))
            (foreach point points
              (setq key (tindump-point-key point))
              (if (not (member key vertices)) (setq vertices (cons key vertices))))))))
    (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
             (= (strcase (tindump-effective-name obj)) (strcase "slope")))
      (progn
        (setq slopes (1+ slopes))
        (setq is-auto-slope (tindump-xdata-type-p obj "tin-slope"))
        (if is-auto-slope
          (setq auto-slopes (1+ auto-slopes)))
        (setq block-point nil)
        (setq raw (vl-catch-all-apply 'vlax-get (list obj 'InsertionPoint)))
        (if (not (vl-catch-all-error-p raw))
          (progn
            (setq raw (tindump-variant-list raw))
            (if (>= (length raw) 2)
              (progn
                (setq block-point (list (car raw) (cadr raw)))
                (if (tindump-point-strictly-in-any-curve-p block-point holes)
                  (setq slopes-in-holes (1+ slopes-in-holes)))))))
        (if block-point
          (if is-auto-slope
            (setq auto-slope-points (cons block-point auto-slope-points))
            (setq extra-slope-records
              (cons
                (list
                  (tindump-object-handle obj)
                  block-point
                  (tindump-first-attribute obj))
                extra-slope-records))))
        (setq slope-values
          (tindump-inc (tindump-first-attribute obj) slope-values))))
    (if (and (not (vl-catch-all-error-p layer)) (= layer "опорыvb")
             (= (vla-get-ObjectName obj) "AcDbBlockReference"))
      (progn
        (setq raw (vl-catch-all-apply 'vlax-get (list obj 'InsertionPoint)))
        (if (not (vl-catch-all-error-p raw))
          (progn
            (setq raw (tindump-variant-list raw))
            (if (>= (length raw) 2)
              (progn
                (setq support-point (list (car raw) (cadr raw)))
                (if (tindump-point-strictly-in-any-curve-p support-point holes)
                  (setq supports-in-holes (1+ supports-in-holes)))))))
        (setq key
          (strcat (itoa (vla-get-Color obj)) "|" (tindump-first-attribute obj)))
        (setq supports (tindump-inc key supports)))))
  (princ "\n===== TINDUMP =====")
  (setq curve-samples
    (if (and (boundp '*opor-session*)
             *opor-session*
             (numberp (opor-session-get 'tin-curve-sample-count)))
      (opor-session-get 'tin-curve-sample-count)
      0))
  (princ (strcat "\nTRI=" (itoa triangles)
                 " XDATA_TIN=" (itoa xdata)
                 " MANUAL_TRI=" (itoa (- triangles xdata))
                 " VERTICES=" (itoa (length vertices))
                 " CURVE_SAMPLES=" (itoa curve-samples)
                 " OUTERS=" (itoa (length contours))
                 " HOLES=" (itoa (length holes))
                 " BAD_CENTROIDS=" (itoa bad-centroids)))
  (setq duplicate-triangles (tindump-duplicate-count signatures))
  (princ (strcat "\nDUP_TRI=" (itoa duplicate-triangles)))
  (foreach signature signatures (princ (strcat "\nTIN " signature)))
  (princ
    (strcat "\nAUTO_MARKS HOLE=" (itoa auto-hole-marks)
            " CURVE=" (itoa auto-curve-marks)))
  (setq slope-values (vl-sort slope-values 'tindump-pair-less-p))
  (princ (strcat "\nSLOPES=" (itoa slopes)
                 " AUTO_TIN=" (itoa auto-slopes)
                 " IN_HOLES=" (itoa slopes-in-holes)))
  (setq extra-overlaps 0)
  (foreach extra-record extra-slope-records
    (if (tindump-point-near-any-p (cadr extra-record) auto-slope-points 1.0)
      (setq extra-overlaps (1+ extra-overlaps))))
  (princ
    (strcat
      "\nNONAUTO_SLOPES=" (itoa (length extra-slope-records))
      " OVERLAP_AUTO=" (itoa extra-overlaps)))
  (foreach extra-record (reverse extra-slope-records)
    (princ
      (strcat
        "\nNONAUTO_SLOPE HANDLE=" (car extra-record)
        " X=" (rtos (car (cadr extra-record)) 2 3)
        " Y=" (rtos (cadr (cadr extra-record)) 2 3)
        " VALUE=" (caddr extra-record)
        " OVERLAP_AUTO="
        (if (tindump-point-near-any-p
              (cadr extra-record) auto-slope-points 1.0)
          "T" "NIL"))))
  (foreach pair slope-values
    (princ (strcat "\nSLOPE " (car pair) "=" (itoa (cdr pair)))))
  (setq supports (vl-sort supports 'tindump-pair-less-p))
  (princ (strcat "\nSUPPORTS=" (itoa (apply '+ (cons 0 (mapcar 'cdr supports))))
                 " IN_HOLES=" (itoa supports-in-holes)))
  (foreach pair supports
    (princ (strcat "\nSUPPORT " (car pair) "=" (itoa (cdr pair)))))
  (princ "\n===== END TINDUMP =====")
  (princ))

(princ "\nprobe_tin загружен. Команда: TINDUMP.")
(princ)
