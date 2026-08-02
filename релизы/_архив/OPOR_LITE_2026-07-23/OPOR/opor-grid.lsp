;;; OPOR grid construction for constant-height MVP

(defun opor-grid-long-line (center axis half-length layer object-type / a b line)
  (setq a (opor-v- center (opor-v* axis half-length)))
  (setq b (opor-v+ center (opor-v* axis half-length)))
  (setq line (vla-AddLine (opor-ms) (vlax-3d-point a) (vlax-3d-point b)))
  (vla-put-Layer line layer)
  (opor-register-created line object-type)
  line)

(defun opor-grid-build-family-range (base axis offset-axis step axis-center half-length idx-min idx-max layer object-type / idx offset center lines)
  (setq lines '())
  (setq idx idx-min)
  (while (<= idx idx-max)
    (setq offset (* idx step))
    (setq center (opor-v+ base (opor-v+ (opor-v* axis axis-center) (opor-v* offset-axis offset))))
    (setq lines (cons (opor-grid-long-line center axis half-length layer object-type) lines))
    (setq idx (1+ idx)))
  (reverse lines))

(defun opor-bbox-corners (bbox / ll ur)
  (setq ll (car bbox))
  (setq ur (cadr bbox))
  (list
    (list (car ll) (cadr ll) 0.0)
    (list (car ur) (cadr ll) 0.0)
    (list (car ur) (cadr ur) 0.0)
    (list (car ll) (cadr ur) 0.0)))

(defun opor-projection-range (points origin axis / values)
  (setq values
    (mapcar
      '(lambda (pt) (opor-dot (opor-v- pt origin) axis))
      points))
  (list (apply 'min values) (apply 'max values)))

(defun opor-index-range-for-bbox (bbox base offset-axis step / range minv maxv)
  (setq range (opor-projection-range (opor-bbox-corners bbox) base offset-axis))
  (setq minv (car range))
  (setq maxv (cadr range))
  (list
    (- (fix (/ minv step)) 3)
    (+ (fix (/ maxv step)) 3)))

(defun opor-axis-center-for-bbox (bbox base axis / range)
  (setq range (opor-projection-range (opor-bbox-corners bbox) base axis))
  (/ (+ (car range) (cadr range)) 2.0))

(defun opor-axis-half-for-bbox (bbox base axis / range)
  (setq range (opor-projection-range (opor-bbox-corners bbox) base axis))
  (+ (/ (- (cadr range) (car range)) 2.0) 1000.0))

(defun opor-lines-total-length (lines / total)
  (setq total 0.0)
  (foreach line lines
    (setq total (+ total (vla-get-Length line))))
  total)

(defun opor-filter-short-grid-lines (lines / kept)
  (setq kept '())
  (foreach line lines
    (if (> (vla-get-Length line) *opor-vba-min-grid-line-length*)
      (setq kept (cons line kept))
      (opor-delete-object line)))
  (reverse kept))

(defun opor-parallel-p (a b / ua ub dotv)
  (setq ua (opor-unit a))
  (setq ub (opor-unit b))
  (if (and ua ub)
    (progn
      (setq dotv (abs (opor-dot ua ub)))
      (> dotv (cos 0.01)))
    nil))

(defun opor-point-to-segment-distance (pt a b / ab len2 tval closest)
  (setq ab (opor-v- b a))
  (setq len2 (opor-dot ab ab))
  (if (equal len2 0.0 1e-12)
    (distance pt a)
    (progn
      (setq tval (/ (opor-dot (opor-v- pt a) ab) len2))
      (if (< tval 0.0) (setq tval 0.0))
      (if (> tval 1.0) (setq tval 1.0))
      (setq closest (opor-v+ a (opor-v* ab tval)))
      (distance pt closest))))

(defun opor-segment-exact-grid-line-p (a b line tol / la lb)
  (setq la (opor-curve-start line))
  (setq lb (opor-curve-end line))
  (or
    (and (< (distance a la) tol) (< (distance b lb) tol))
    (and (< (distance a lb) tol) (< (distance b la) tol))))

(defun opor-segment-start-on-grid-line-p (a b line tol / la lb)
  (setq la (opor-curve-start line))
  (setq lb (opor-curve-end line))
  (and
    (opor-parallel-p (opor-v- b a) (opor-v- lb la))
    (< (opor-point-to-segment-distance a la lb) tol)))

(defun opor-segment-duplicates-grid-p (a b lag-lines mode / found)
  (setq found nil)
  (foreach line lag-lines
    (if (not found)
      (cond
        ((= mode "outer")
          (if (opor-segment-exact-grid-line-p a b line 0.1)
            (setq found T)))
        ((= mode "hole")
          (if (opor-segment-start-on-grid-line-p a b line 1.0)
            (setq found T)))
        (t
          (if (opor-segment-start-on-grid-line-p a b line 1.0)
            (setq found T))))))
  found)

(defun opor-list-tail (lst)
  (while (cdr lst)
    (setq lst (cdr lst)))
  (car lst))

(defun opor-polyline-bulge (pline idx / value)
  (setq value (vl-catch-all-apply 'vla-GetBulge (list pline idx)))
  (if (vl-catch-all-error-p value) 0.0 value))

(defun opor-polyline-line-edge-pairs (pline / pts result idx a b first tail-point)
  (setq pts (opor-polyline-points pline))
  (setq result '())
  (setq idx 0)
  (while (cdr pts)
    (if (equal (opor-polyline-bulge pline idx) 0.0 1e-9)
      (setq result (cons (list (opor-2d (car pts)) (opor-2d (cadr pts))) result)))
    (setq pts (cdr pts))
    (setq idx (1+ idx)))
  (setq result (reverse result))
  (if (and (opor-polyline-closed-p pline) result)
    (progn
      (setq first (caar result))
      (setq tail-point (cadr (opor-list-tail result)))
      (if (and
            (> (distance first tail-point) 1e-7)
            (equal (opor-polyline-bulge pline idx) 0.0 1e-9))
        (setq result (append result (list (list tail-point first)))))))
  result)

;; b2_mains: рёбра контура/проёмов без фильтра длины — угол (параллельность
;; лагам) + дедуп против сетки; выжившие ФИЗИЧЕСКИ переносятся на сеткаvb
(defun opor-boundary-lag-segments (pline lag-axis-vector lag-lines mode / segs pair a b seg)
  (setq segs '())
  (foreach pair (opor-polyline-line-edge-pairs pline)
    (setq a (car pair))
    (setq b (cadr pair))
    (setq seg (opor-v- b a))
    (if (and
          (> (distance a b) 1e-8)
          (opor-parallel-p seg lag-axis-vector)
          (not (opor-segment-duplicates-grid-p a b lag-lines mode)))
      (setq segs (cons pair segs))))
  (reverse segs))

(defun opor-holes-lag-segments (holes lag-axis-vector lag-lines / segs)
  (setq segs '())
  (foreach hole holes
    (setq segs (append segs (opor-boundary-lag-segments hole lag-axis-vector lag-lines "hole"))))
  segs)

(defun opor-segments-total-length (segs / total)
  (setq total 0.0)
  (foreach pair segs
    (setq total (+ total (distance (car pair) (cadr pair)))))
  total)

(defun opor-create-boundary-lag-lines (segs / lines)
  (setq lines '())
  (foreach pair segs
    (setq lines
      (cons
        (opor-make-line (car pair) (cadr pair) *opor-layer-grid* "grid-edge")
        lines)))
  (reverse lines))

(defun opor-grid-line-endpoints (lines / points)
  (setq points '())
  (foreach line lines
    (setq points (cons (opor-curve-start line) points))
    (setq points (cons (opor-curve-end line) points)))
  points)

(defun opor-grid-node-points (v-lines p-lines boundary holes / points ints)
  (setq points '())
  (foreach vline v-lines
    (foreach pline p-lines
      (setq ints (opor-obj-intersections vline pline))
      (foreach pt ints
        (if (opor-point-in-working-area-p pt boundary holes)
          (setq points (cons pt points))))))
  points)

;; UX v3.7: авто-дефолт — вершина длиннейшего прямого ребра + направление вдоль него;
;; контур целиком из дуг -> первая вершина + ось X
(defun opor-grid-default-base-dir (boundary / best best-len len first-pt)
  (setq best nil)
  (setq best-len 0.0)
  (foreach pair (opor-polyline-line-edge-pairs boundary)
    (setq len (distance (car pair) (cadr pair)))
    (if (> len best-len)
      (progn (setq best pair) (setq best-len len))))
  (if best
    (list (car best) (cadr best))
    (progn
      (setq first-pt (opor-2d (car (opor-polyline-vertices boundary))))
      (list first-pt (opor-v+ first-pt (list 1000.0 0.0 0.0))))))

;; ТЗ П5: лага = ряд; отрезки одного ряда (разрезанные проёмом) — одна лага.
;; Ключ ряда — смещение начала линии поперёк направления лаг, округлённое до 1 мм.
(defun opor-lag-row-count (lines base row-axis / off key seen)
  (setq seen '())
  (foreach line lines
    (setq off (opor-dot (opor-v- (opor-curve-start line) base) row-axis))
    (setq key (opor-round off))
    (if (not (member key seen)) (setq seen (cons key seen))))
  (length seen))

(defun opor-grid-build (session / boundary holes base dir step-x step-y vec perp bbox diag half range-v range-p center-v center-p half-v half-p raw-v raw-p v-lines p-lines lag-axis lag-lines lag-axis-vector uses-lags grid-length outer-segs hole-segs boundary-outer-length boundary-holes-length boundary-length lag-length grid edge-lines row-axis perp-lines dbl-step dbl-res dbl-layout lag-width secondary-lines node-points endpoint-points stagger-points support-step)
  (setq boundary (opor-session-get 'outer-boundary))
  (setq holes (opor-session-get 'holes))
  (setq base (opor-session-get 'base-point))
  (setq dir (opor-session-get 'direction-point))
  (setq step-x (opor-session-get 'step-x))
  (setq step-y (opor-session-get 'step-y))
  (setq vec (opor-unit (opor-v- dir base)))
  (if (not vec)
    (progn
      (opor-alert "Нулевое направление сетки.")
      nil)
    (progn
      (setq perp (opor-perp2d vec))
      (setq bbox (opor-bbox boundary))
      (setq diag (max 1.0 (opor-bbox-diagonal bbox)))
      (setq half (* diag *opor-grid-margin-factor*))
      (setq range-v (opor-index-range-for-bbox bbox base perp step-y))
      (setq range-p (opor-index-range-for-bbox bbox base vec step-x))
      (setq center-v (opor-axis-center-for-bbox bbox base vec))
      (setq center-p (opor-axis-center-for-bbox bbox base perp))
      (setq half-v (max half (opor-axis-half-for-bbox bbox base vec)))
      (setq half-p (max half (opor-axis-half-for-bbox bbox base perp)))
      (opor-session-set 'grid-v-index-range range-v)
      (opor-session-set 'grid-p-index-range range-p)
      (setq raw-v
        (opor-grid-build-family-range
          base vec perp step-y center-v half-v (car range-v) (cadr range-v) *opor-layer-grid* "grid-v-raw"))
      (setq raw-p
        (opor-grid-build-family-range
          base perp vec step-x center-p half-p (car range-p) (cadr range-p) *opor-layer-grid* "grid-p-raw"))
      (setq v-lines (opor-trim-lines-by-boundaries raw-v boundary holes "grid-v"))
      (setq p-lines (opor-trim-lines-by-boundaries raw-p boundary holes "grid-p"))
      (setq v-lines (opor-filter-short-grid-lines v-lines))
      (setq p-lines (opor-filter-short-grid-lines p-lines))
      (setq lag-axis (opor-session-get 'lag-axis))
      (setq lag-lines
        (if (= lag-axis "perp")
          p-lines
          v-lines))
      (setq lag-axis-vector
        (if (= lag-axis "perp")
          perp
          vec))
      (setq uses-lags (opor-floor-uses-lags-p))
      (setq grid-length (opor-lines-total-length lag-lines))
      (setq secondary-lines '())
      ;; S6: сдвоенные лаги (copxlinlag) - доп. семейство параллельно лагам.
      ;; Длина входит в LENGTH ДО дедупа (квирк VBA trimXL); выжившие линии
      ;; вливаются в лаговое семейство: рёбра/узлы/опоры/ряды считают их сами.
      (setq dbl-step (opor-session-get 'double-lag-step))
      (setq dbl-layout (opor-session-get 'double-lag-layout))
      (cond
        ;; ТЗ П9: в местах стыка старый центральный ряд заменяется физической
        ;; парой; вторая лага пары получает отдельные шахматные точки опор.
        ((and uses-lags
           (member dbl-layout '("even" "half"))
           (numberp dbl-step) (> dbl-step 0.0))
          (setq lag-width (opor-session-get 'lag-width))
          (if (or (not (numberp lag-width)) (<= lag-width 0.0))
            (setq lag-width (cdr (assoc 'lag-width *opor-default-params*))))
          (setq dbl-res
            (if (= lag-axis "perp")
              (opor-dbl-lag-layout-apply
                base perp vec dbl-step lag-width center-p half-p bbox boundary holes lag-lines)
              (opor-dbl-lag-layout-apply
                base vec perp dbl-step lag-width center-v half-v bbox boundary holes lag-lines)))
          (setq lag-lines (cdr (assoc 'lag-lines dbl-res)))
          (setq secondary-lines (cdr (assoc 'secondary-lines dbl-res)))
          (setq grid-length (cdr (assoc 'lag-length-mm dbl-res)))
          (if (= lag-axis "perp")
            (setq p-lines lag-lines)
            (setq v-lines lag-lines)))
        ;; Старый ручной double-lag-step остаётся как скрытый compatibility-path
        ;; для прежних сценариев и эталонов S6.
        ((and uses-lags (numberp dbl-step) (> dbl-step 0.0))
          (setq dbl-res
            (if (= lag-axis "perp")
              (opor-dbl-lag-apply base perp vec dbl-step center-p half-p bbox boundary holes lag-lines)
              (opor-dbl-lag-apply base vec perp dbl-step center-v half-v bbox boundary holes lag-lines)))
          (setq grid-length (+ grid-length (cdr dbl-res)))
          (setq lag-lines (append lag-lines (car dbl-res)))
          (if (= lag-axis "perp")
            (setq p-lines (append p-lines (car dbl-res)))
            (setq v-lines (append v-lines (car dbl-res))))))
      (setq outer-segs
        (if uses-lags
          (opor-boundary-lag-segments boundary lag-axis-vector lag-lines "outer")
          '()))
      (setq hole-segs
        (if uses-lags
          (opor-holes-lag-segments holes lag-axis-vector lag-lines)
          '()))
      (setq boundary-outer-length (opor-segments-total-length outer-segs))
      (setq boundary-holes-length (opor-segments-total-length hole-segs))
      (setq boundary-length (+ boundary-outer-length boundary-holes-length))
      ;; физическое создание рёбер-лаг на сеткаvb (как explode в b2_mains)
      (setq edge-lines
        (cond
          ((not uses-lags) '())
          ((= *opor-boundary-lag-length-mode* "all")
            (opor-create-boundary-lag-lines (append outer-segs hole-segs)))
          ((= *opor-boundary-lag-length-mode* "holes")
            (opor-create-boundary-lag-lines hole-segs))
          (t '())))
      (setq lag-length
        (cond
          ((not uses-lags) 0.0)
          ((= *opor-boundary-lag-length-mode* "all")
            (+ grid-length boundary-length))
          ((= *opor-boundary-lag-length-mode* "holes")
            (+ grid-length boundary-holes-length))
           (t
             grid-length)))
      (setq node-points (opor-grid-node-points v-lines p-lines boundary holes))
      (setq endpoint-points
        (append (opor-grid-line-endpoints v-lines)
                (opor-grid-line-endpoints p-lines)))
      (if secondary-lines
        (progn
          (setq node-points
            (opor-dbl-lag-remove-secondary-nodes node-points secondary-lines))
          (setq support-step (if (= lag-axis "perp") step-y step-x))
          (setq stagger-points
            (opor-dbl-lag-stagger-points
              secondary-lines base lag-axis-vector support-step boundary holes))
          (setq node-points (append node-points stagger-points))
          (opor-session-set 'dbl-lag-stagger-support-count (length stagger-points)))
        (opor-session-set 'dbl-lag-stagger-support-count 0))
      (setq grid
        (list
          (cons 'v-lines v-lines)
          (cons 'p-lines p-lines)
          (cons 'lag-lines (if uses-lags lag-lines '()))
          (cons 'grid-lag-length-mm (if uses-lags grid-length 0.0))
          (cons 'boundary-outer-lag-length-mm boundary-outer-length)
          (cons 'boundary-holes-lag-length-mm boundary-holes-length)
          (cons 'boundary-lag-length-mm boundary-length)
          (cons 'lag-length-mm lag-length)
          (cons 'node-points node-points)
          (cons 'endpoint-points endpoint-points)))
      (opor-session-set 'grid grid)
      (opor-session-set 'grid-lag-length-mm (if uses-lags grid-length 0.0))
      (opor-session-set 'boundary-outer-lag-length-mm boundary-outer-length)
      (opor-session-set 'boundary-holes-lag-length-mm boundary-holes-length)
      (opor-session-set 'boundary-lag-length-mm boundary-length)
      (opor-session-set 'lag-length-mm lag-length)
      ;; b3_fin: gridleng = Round(gridleng / 1000, 0) — банковское
      (opor-session-set 'lag-length-m (opor-round-half-even (/ lag-length 1000.0)))
      ;; ТЗ П5: счётчик лаг (рядов) — по лаг-линиям и физическим рёбрам
      (setq row-axis (if (= lag-axis "perp") vec perp))
      (opor-session-set 'lag-row-count
        (if uses-lags
          (opor-lag-row-count (append lag-lines edge-lines) base row-axis)
          0))
      ;; ТЗ П5: в чертеже остаются только лаги — поперечное семейство
      ;; удаляется ПОСЛЕ снятия node-points/endpoint-points (они уже в grid)
      (setq perp-lines (if (= lag-axis "perp") v-lines p-lines))
      (cond
        ;; LASTRA: обе семьи нужны только для получения точек опор. Физических
        ;; лаг и их длины в таком составе пола нет.
        ((not uses-lags)
          (setq perp-lines (append v-lines p-lines))
          (foreach line perp-lines
            (opor-delete-object line)
            (opor-unregister-created line))
          (opor-session-set 'perp-lines-removed (length perp-lines))
          (setq grid
            (subst (cons 'v-lines '()) (assoc 'v-lines grid) grid))
          (setq grid
            (subst (cons 'p-lines '()) (assoc 'p-lines grid) grid))
          (opor-session-set 'grid grid))
        ((not *opor-keep-perp-grid*)
          (foreach line perp-lines
            (opor-delete-object line)
            (opor-unregister-created line))
          (opor-session-set 'perp-lines-removed (length perp-lines))
          (setq grid
            (subst
              (cons (if (= lag-axis "perp") 'v-lines 'p-lines) '())
              (assoc (if (= lag-axis "perp") 'v-lines 'p-lines) grid)
              grid))
          (opor-session-set 'grid grid))
        (t (opor-session-set 'perp-lines-removed 0)))
      grid)))

(princ)
