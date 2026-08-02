;;; OPOR S6: сдвоенные лаги - порт copxlinlag + дедуп "#" из trimXL.
;;; 1:1 с VBA, включая квирк: длина доп. лаг входит в LENGTH ДО дедупа
;;; (trimXL суммирует grlengCur до удаления совпавших линий).

;; Семейство доп. лаг: параллельно лаговому, шаг step, индексы диапазона bbox
;; БЕЗ нуля (VBA copyxlinlag: q начинается с 1, на базовой позиции линии нет).
(defun opor-dbl-lag-raw-lines (base axis offset-axis step axis-center half-length bbox / range)
  (setq range (opor-index-range-for-bbox bbox base offset-axis step))
  (append
    (opor-grid-build-family-range
      base axis offset-axis step axis-center half-length
      (car range) -1 *opor-layer-grid* "grid-dbl-raw")
    (opor-grid-build-family-range
      base axis offset-axis step axis-center half-length
      1 (cadr range) *opor-layer-grid* "grid-dbl-raw")))

;; trimXL: |dx|<10 и |dy|<10 покоординатно, старт-к-старту и конец-к-концу
;; (ориентация линий одного семейства у порта детерминирована, разворот не нужен)
(defun opor-dbl-lag-coincides-p (a b tol / sa ea sb eb)
  (setq sa (opor-curve-start a))
  (setq ea (opor-curve-end a))
  (setq sb (opor-curve-start b))
  (setq eb (opor-curve-end b))
  (and
    (< (abs (- (car sa) (car sb))) tol)
    (< (abs (- (cadr sa) (cadr sb))) tol)
    (< (abs (- (car ea) (car eb))) tol)
    (< (abs (- (cadr ea) (cadr eb))) tol)))

;; Полный цикл S6: построить raw, обрезать по контуру/проёмам, отфильтровать
;; короткие, покрасить в цвет 12, посчитать длину ДО дедупа (квирк VBA),
;; удалить совпавшие с основной сеткой. Возвращает (выжившие . длина-мм-до-дедупа).
(defun opor-dbl-lag-apply (base axis offset-axis step axis-center half-length bbox boundary holes lag-lines
                           / raw trimmed survivors removed len)
  (setq raw (opor-dbl-lag-raw-lines base axis offset-axis step axis-center half-length bbox))
  (setq trimmed (opor-trim-lines-by-boundaries raw boundary holes "grid-dbl"))
  (setq trimmed (opor-filter-short-grid-lines trimmed))
  (foreach line trimmed
    (vl-catch-all-apply 'vla-put-Color (list line *opor-dbl-lag-color*)))
  (setq len (opor-lines-total-length trimmed))
  (setq survivors '())
  (setq removed 0)
  (foreach line trimmed
    (if (vl-some
          '(lambda (main) (opor-dbl-lag-coincides-p line main *opor-vba-dbl-lag-dedupe-tolerance*))
          lag-lines)
      (progn
        (opor-delete-object line)
        (opor-unregister-created line)
        (setq removed (1+ removed)))
      (setq survivors (cons line survivors))))
  (opor-session-set 'dbl-lag-created (length trimmed))
  (opor-session-set 'dbl-lag-deduped removed)
  (cons (reverse survivors) len))

;; ---------- ТЗ П9: пары лаг по длине/раскладке доски ----------

(defun opor-dbl-lag-line-row-offset (line base row-axis)
  (opor-dot (opor-v- (opor-curve-start line) base) row-axis))

(defun opor-dbl-lag-joint-offsets (bbox base row-axis step / range idx-min idx-max idx result)
  (setq range (opor-projection-range (opor-bbox-corners bbox) base row-axis))
  (setq idx-min (- (fix (/ (car range) step)) 2))
  (setq idx-max (+ (fix (/ (cadr range) step)) 2))
  (setq idx idx-min)
  (setq result '())
  (while (<= idx idx-max)
    ;; В базовой точке начинается первая доска, стыка там нет.
    (if (/= idx 0)
      (setq result (cons (* idx step) result)))
    (setq idx (1+ idx)))
  (reverse result))

(defun opor-dbl-lag-row-at-joint-p (line base row-axis joints tol / off)
  (setq off (opor-dbl-lag-line-row-offset line base row-axis))
  (vl-some
    '(lambda (joint) (< (abs (- off joint)) tol))
    joints))

(defun opor-dbl-lag-pair-raw-lines (base axis row-axis joints width axis-center half-length
                                    / primary secondary joint center)
  (setq primary '())
  (setq secondary '())
  (foreach joint joints
    (setq center
      (opor-v+ base
        (opor-v+ (opor-v* axis axis-center)
                 (opor-v* row-axis (- joint (/ width 2.0))))))
    (setq primary
      (cons (opor-grid-long-line center axis half-length *opor-layer-grid* "grid-dbl-primary-raw") primary))
    (setq center
      (opor-v+ base
        (opor-v+ (opor-v* axis axis-center)
                 (opor-v* row-axis (+ joint (/ width 2.0))))))
    (setq secondary
      (cons (opor-grid-long-line center axis half-length *opor-layer-grid* "grid-dbl-secondary-raw") secondary)))
  (list (reverse primary) (reverse secondary)))

;; Для нового режима старый одиночный ряд в центре стыка заменяется двумя
;; физическими лагами по сторонам стыка. Возвращается alist с новой семьёй.
(defun opor-dbl-lag-layout-apply (base axis row-axis joint-step width axis-center half-length bbox boundary holes lag-lines
                                  / joints kept removed-main raw primary secondary all-lines)
  (setq joints (opor-dbl-lag-joint-offsets bbox base row-axis joint-step))
  (setq kept '())
  (setq removed-main 0)
  (foreach line lag-lines
    (if (opor-dbl-lag-row-at-joint-p line base row-axis joints *opor-vba-dbl-lag-dedupe-tolerance*)
      (progn
        (opor-delete-object line)
        (opor-unregister-created line)
        (setq removed-main (1+ removed-main)))
      (setq kept (cons line kept))))
  (setq kept (reverse kept))
  (setq raw
    (opor-dbl-lag-pair-raw-lines
      base axis row-axis joints width axis-center half-length))
  (setq primary
    (opor-filter-short-grid-lines
      (opor-trim-lines-by-boundaries (car raw) boundary holes "grid-dbl-primary")))
  (setq secondary
    (opor-filter-short-grid-lines
      (opor-trim-lines-by-boundaries (cadr raw) boundary holes "grid-dbl-secondary")))
  (foreach line (append primary secondary)
    (vl-catch-all-apply 'vla-put-Color (list line *opor-dbl-lag-color*)))
  (setq all-lines (append kept primary secondary))
  (opor-session-set 'dbl-lag-created (+ (length primary) (length secondary)))
  (opor-session-set 'dbl-lag-deduped removed-main)
  (opor-session-set 'dbl-lag-joint-count
    (opor-lag-row-count primary base row-axis))
  (opor-session-set 'dbl-lag-pair-segments (+ (length primary) (length secondary)))
  (list
    (cons 'lag-lines all-lines)
    (cons 'primary-lines primary)
    (cons 'secondary-lines secondary)
    (cons 'removed-main-count removed-main)
    (cons 'lag-length-mm (opor-lines-total-length all-lines))))

(defun opor-dbl-lag-point-near-lines-p (pt lines tol / found)
  (setq found nil)
  (foreach line lines
    (if (and
          (not found)
          (< (opor-point-to-segment-distance
               pt (opor-curve-start line) (opor-curve-end line))
             tol))
      (setq found T)))
  found)

(defun opor-dbl-lag-remove-secondary-nodes (nodes secondary-lines / result)
  (setq result '())
  (foreach pt nodes
    (if (not (opor-dbl-lag-point-near-lines-p pt secondary-lines 1.0))
      (setq result (cons pt result))))
  (reverse result))

;; Точки второй лаги идут по глобальному ряду (n + 1/2) * шаг, поэтому
;; соседние лаги получают требуемый шахматный сдвиг на половину шага опор.
(defun opor-dbl-lag-line-stagger-points (line base axis support-step boundary holes
                                         / a b ta tb minv maxv idx-min idx-max idx target pt result)
  (setq a (opor-curve-start line))
  (setq b (opor-curve-end line))
  (setq ta (opor-dot (opor-v- a base) axis))
  (setq tb (opor-dot (opor-v- b base) axis))
  (setq minv (min ta tb))
  (setq maxv (max ta tb))
  (setq idx-min (- (fix (/ minv support-step)) 2))
  (setq idx-max (+ (fix (/ maxv support-step)) 2))
  (setq idx idx-min)
  (setq result '())
  (while (<= idx idx-max)
    (setq target (* (+ idx 0.5) support-step))
    (if (and (>= target (- minv 1e-7)) (<= target (+ maxv 1e-7)))
      (progn
        (setq pt (opor-v+ a (opor-v* axis (- target ta))))
        (if (opor-point-in-working-area-p pt boundary holes)
          (setq result (cons pt result)))))
    (setq idx (1+ idx)))
  (reverse result))

(defun opor-dbl-lag-stagger-points (lines base axis support-step boundary holes / result)
  (setq result '())
  (foreach line lines
    (setq result
      (append result
        (opor-dbl-lag-line-stagger-points
          line base axis support-step boundary holes))))
  result)

(defun opor-dbl-lag-log-text (/ layout step)
  (setq layout (opor-session-get 'double-lag-layout))
  (setq step (opor-session-get 'double-lag-step))
  (if (and (member layout '("even" "half")) (numberp step) (> step 0.0))
    (strcat
      ", П9: доска=" (rtos (opor-session-get 'board-length) 2 0)
      ", раскладка=" (if (= layout "half") "1/2" "ровно")
      ", стык=" (rtos step 2 0)
      ", парных сегментов=" (itoa (opor-dump-session-int 'dbl-lag-pair-segments))
      ", шахматных опор=" (itoa (opor-dump-session-int 'dbl-lag-stagger-support-count)))
    ""))

(princ)
