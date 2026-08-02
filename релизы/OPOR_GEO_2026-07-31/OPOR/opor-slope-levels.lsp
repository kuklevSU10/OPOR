;;; OPOR: автоматическая расстановка отметок по процентам и областям уклона.
;;; Один выбранный блок отметки задаёт базовую высоту. Каждая замкнутая область
;;; на линии_высот должна содержать ровно один блок slope. Направление стрелки
;;; slope считается направлением повышения отметки.

(defun opor-slope-level-area-target-p (area boundary / found pt)
  (setq found nil)
  (foreach pt (opor-polyline-vertices area)
    (if (and (not found)
             (or (opor-point-inside-boundary-p (opor-2d pt) boundary)
                 (opor-point-on-curve-p
                   (opor-2d pt) boundary *opor-slope-boundary-tolerance*)))
      (setq found T)))
  (if (and (not found) (opor-obj-intersections area boundary))
    (setq found T))
  found)

(defun opor-slope-level-areas (boundary / result obj)
  (setq result '())
  (vlax-for obj (opor-ms)
    (if (and
          (not (eq obj boundary))
          (opor-polyline-object-p obj)
          (= (strcase (vla-get-Layer obj))
             (strcase *opor-layer-level-lines*))
          (opor-polyline-closed-p obj)
          (opor-slope-level-area-target-p obj boundary))
      (setq result (cons obj result))))
  (reverse result))

(defun opor-slope-level-exact-percent (block visible / en data app values exact)
  (setq en (opor-object-ename block))
  (if (and (= (opor-object-xdata-type block) "tin-slope")
           en (not (vl-catch-all-error-p en)))
    (progn
      (setq data (entget en (list *opor-xdata-app*)))
      (setq app (cadr (assoc -3 data)))
      (if (and app (= (car app) *opor-xdata-app*))
        (progn
          (setq values
            (vl-remove-if-not
              '(lambda (item) (and (listp item) (= (car item) 1040)))
              (cdr app)))
          (if values (setq exact (cdr (car values))))))))
  ;; Авто-TIN показывает три знака. Если пользователь поменял атрибут заметнее
  ;; половины последнего знака, считаем новое видимое значение главным.
  (if (and (numberp exact) (numberp visible)
           (<= (abs (- exact visible)) 0.0005001))
    exact
    visible))

(defun opor-slope-level-records (boundary / bbox blocks result block pt percent)
  (setq bbox (opor-bbox boundary))
  (setq blocks (opor-slope-crossing-blocks bbox))
  (setq result '())
  (foreach block (opor-slope-filter-blocks blocks "slope")
    (setq pt (opor-slope-insertion-point block))
    (if (and pt
             (or (opor-point-inside-boundary-p (opor-2d pt) boundary)
                 (opor-point-on-curve-p
                   (opor-2d pt) boundary *opor-slope-boundary-tolerance*)))
      (progn
        (setq percent
          (opor-slope-level-exact-percent
            block (opor-slope-number-from-block block)))
        (setq result
          (cons
            (list
              (cons 'object block)
              (cons 'point (opor-2d pt))
              (cons 'percent percent))
            result)))))
  (reverse result))

(defun opor-slope-level-records-in-area (records area / result record pt)
  (setq result '())
  (foreach record records
    (setq pt (cdr (assoc 'point record)))
    ;; Как в текущих Slope/%: блок на самой границе областью не считается.
    (if (and pt (opor-point-inside-boundary-p pt area))
      (setq result (cons record result))))
  (reverse result))

;; Отметки читаем прямо из ModelSpace: так результат не зависит от состояния
;; слоя и от того, выходит ли крайняя область разуклонки за bbox контура.
(defun opor-slope-level-marks (/ result obj pt text height)
  (setq result '())
  (vlax-for obj (opor-ms)
    (if (opor-slope-block-name-p obj *opor-level-block-name*)
      (progn
        (setq pt (opor-slope-insertion-point obj))
        (setq text (opor-first-attribute-text obj))
        (setq height (opor-parse-real text nil))
        (if pt
          (setq result
            (cons
              (list
                (cons 'object obj)
                (cons 'point (opor-2d pt))
                (cons 'height height))
              result))))))
  (reverse result))

(defun opor-slope-level-point-index (pt points tol / index found)
  (setq index 0 found nil)
  (while (and points (not found))
    (if (<= (distance (opor-2d pt) (opor-2d (car points))) tol)
      (setq found index)
      (progn
        (setq index (1+ index))
        (setq points (cdr points)))))
  found)

(defun opor-slope-level-node-indices (area points / result pt index)
  (setq result '())
  (foreach pt (opor-polyline-vertices area)
    (setq index (opor-slope-level-point-index pt points 1.0))
    (if (and (numberp index) (not (member index result)))
      (setq result (append result (list index)))))
  result)

(defun opor-slope-level-axis-unit (block / angle)
  (setq angle (opor-write-level-axis-angle block))
  (list (cos angle) (sin angle) 0.0))

(defun opor-slope-level-known-height (index known / pair)
  (setq pair (assoc index known))
  (if pair (cdr pair) nil))

(defun opor-slope-level-first-source (indices known conflicts / found index)
  (setq found nil)
  (foreach index indices
    (if (and (not found)
             (assoc index known)
             (not (member index conflicts)))
      (setq found index)))
  found)

;; Возвращает (known conflicts added-p). Первое значение остаётся рабочим,
;; но вершина помечается конфликтной, если другой путь отличается больше tol.
(defun opor-slope-level-put-known (index value known conflicts tol / pair added)
  (setq pair (assoc index known))
  (setq added nil)
  (if pair
    (if (> (abs (- (cdr pair) value)) tol)
      (if (not (member index conflicts))
        (setq conflicts (cons index conflicts))))
    (progn
      (setq known (cons (cons index value) known))
      (setq added T)))
  (list known conflicts added))

(defun opor-slope-level-candidate
  (source-index target-index source-height points percent axis / projection delta)
  (setq projection
    (opor-dot
      (opor-v-
        (nth target-index points)
        (nth source-index points))
      axis))
  ;; Не округляем промежуточные переходы: иначе цепочка треугольников
  ;; накапливает миллиметровую ошибку. Округление выполняется только при записи.
  (setq delta (/ (* percent projection) 100.0))
  (+ source-height delta))

(defun opor-slope-level-propagate
  (areas points known tol / changed pass limit area indices source source-height
                         percent axis index value put conflicts)
  (setq conflicts '())
  (setq changed T pass 0 limit (+ (length points) (length areas) 5))
  (while (and changed (< pass limit))
    (setq changed nil pass (1+ pass))
    (foreach area areas
      (setq indices (cdr (assoc 'indices area)))
      (setq source
        (opor-slope-level-first-source indices known conflicts))
      (if (numberp source)
        (progn
          (setq source-height (opor-slope-level-known-height source known))
          (setq percent (cdr (assoc 'percent area)))
          (setq axis (cdr (assoc 'axis area)))
          (foreach index indices
            (setq value
              (opor-slope-level-candidate
                source index source-height points percent axis))
            (setq put
              (opor-slope-level-put-known
                index value known conflicts tol))
            (setq known (nth 0 put))
            (setq conflicts (nth 1 put))
            (if (nth 2 put) (setq changed T)))))))
  (list known (reverse conflicts) pass))

(defun opor-slope-level-unresolved-indices (points known / index result pt)
  (setq index 0 result '())
  (foreach pt points
    (if (not (assoc index known))
      (setq result (cons index result)))
    (setq index (1+ index)))
  (reverse result))

(defun opor-slope-level-index-points (indices points / result index)
  (setq result '())
  (foreach index indices
    (setq result (cons (nth index points) result)))
  (reverse result))

(defun opor-slope-level-text (value plus-p)
  (setq value (opor-round-half-even value))
  (strcat
    (if (and plus-p (>= value 0.0)) "+" "")
    (opor-height-text value)))

(defun opor-slope-level-insert-mark (pt text / value block)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (opor-ms) (vlax-3d-point (opor-2d pt)) *opor-level-block-name*
        1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    nil
    (progn
      (setq block value)
      (opor-support-set-first-attribute block text)
      (opor-register-created block "slope-level-mark")
      block)))

(defun opor-slope-level-write
  (points known marks plus-p / index pt height mark object text inserted updated failed)
  (setq index 0 inserted 0 updated 0 failed 0)
  (foreach pt points
    (setq height (opor-slope-level-known-height index known))
    (setq mark (opor-level-mark-at-point marks pt 1.0))
    (setq text (opor-slope-level-text height plus-p))
    (if mark
      (progn
        (setq object (cdr (assoc 'object mark)))
        (if object
          (progn
            (opor-support-set-first-attribute object text)
            (setq updated (1+ updated)))
          (setq failed (1+ failed))))
      (if (opor-slope-level-insert-mark pt text)
        (setq inserted (1+ inserted))
        (setq failed (1+ failed))))
    (setq index (1+ index)))
  (list inserted updated failed))

(defun opor-slope-level-run
  (/ boundary reference reference-text reference-height reference-point plus-p
     areas slope-records points area matches record percent area-records
     invalid invalid-points indices axis reference-index propagated known
     conflicts unresolved marks write-result)
  (cond
    ((not (opor-layer-exists-p *opor-layer-level-lines*))
      (opor-alert "Не найден слой линии_высот.")
      nil)
    ((not (opor-import-level-block))
      (opor-alert
        "Не найден блок отметки otmetka_oporvb и не удалось загрузить его из библиотеки.")
      nil)
    ((not (opor-block-exists-p "slope"))
      (opor-alert "Не найден блок уклона slope.")
      nil)
    (t
      (opor-view-save)
      (setq boundary (opor-auto-level-pick-boundary))
      (if (not boundary)
        nil
        (if (not (and (opor-polyline-closed-p boundary)
                      (opor-polyline-region-valid-p boundary "Контур")))
          (progn
            (opor-alert "Для расчёта нужен замкнутый корректный внешний контур.")
            nil)
          (progn
          (opor-zoom-to-boundary boundary)
          (setq reference
            (opor-write-level-pick-block
              "\nУкажите одну известную базовую отметку: "
              *opor-level-block-name*
              "Это не блок высотной отметки."))
          (if (not reference)
            nil
            (progn
              (setq reference-text (opor-first-attribute-text reference))
              (setq reference-height (opor-parse-real reference-text nil))
              (setq reference-point (opor-slope-insertion-point reference))
              (setq plus-p
                (and reference-text
                     (not (null (vl-string-search "+" reference-text)))))
              (setq areas (opor-slope-level-areas boundary))
              (setq slope-records (opor-slope-level-records boundary))
              (cond
                ((not (numberp reference-height))
                  (opor-alert "В базовой отметке нет числового значения.")
                  nil)
                ((not reference-point)
                  (opor-alert "Не удалось прочитать точку базовой отметки.")
                  nil)
                ((not areas)
                  (opor-alert "Не найдены замкнутые области разуклонки на слое линии_высот.")
                  nil)
                ((not slope-records)
                  (opor-alert "В выбранном объекте не найдены блоки slope.")
                  nil)
                (t
                  (setq points '())
                  (foreach area areas
                    (setq points
                      (append points (opor-polyline-vertices area))))
                  (setq points (opor-auto-level-unique-points points))
                  (setq area-records '() invalid 0 invalid-points '())
                  (foreach area areas
                    (setq matches
                      (opor-slope-level-records-in-area slope-records area))
                    (setq record (if (= (length matches) 1) (car matches) nil))
                    (setq percent
                      (if record (cdr (assoc 'percent record)) nil))
                    (setq indices
                      (opor-slope-level-node-indices area points))
                    (if (and record (numberp percent) (>= percent 0.0)
                             (>= (length indices) 3))
                      (progn
                        (setq axis
                          (opor-slope-level-axis-unit
                            (cdr (assoc 'object record))))
                        (setq area-records
                          (cons
                            (list
                              (cons 'object area)
                              (cons 'indices indices)
                              (cons 'percent percent)
                              (cons 'axis axis))
                            area-records)))
                      (progn
                        (setq invalid (1+ invalid))
                        (if (opor-polyline-vertices area)
                          (setq invalid-points
                            (cons (car (opor-polyline-vertices area))
                                  invalid-points))))))
                  (setq area-records (reverse area-records))
                  (if (> invalid 0)
                    (progn
                      (opor-error-circles invalid-points)
                      (opor-alert
                        (strcat
                          "Расчёт не выполнен. В каждой области должен быть ровно один "
                          "блок slope с неотрицательным процентом.\nПроблемных областей: "
                          (itoa invalid) "."))
                      nil)
                    (progn
                      (setq reference-index
                        (opor-slope-level-point-index
                          reference-point points 1.0))
                      (if (not (numberp reference-index))
                        (progn
                          (opor-error-circle reference-point)
                          (opor-alert
                            "Базовая отметка должна стоять точно в вершине области разуклонки.")
                          nil)
                        (progn
                          (setq propagated
                            (opor-slope-level-propagate
                              area-records points
                              (list (cons reference-index reference-height))
                              1.0))
                          (setq known (nth 0 propagated))
                          (setq conflicts (nth 1 propagated))
                          (setq unresolved
                            (opor-slope-level-unresolved-indices points known))
                          (cond
                            (conflicts
                              (opor-error-circles
                                (opor-slope-level-index-points conflicts points))
                              (opor-alert
                                (strcat
                                  "Расчёт не записан: разные пути дают отметки с "
                                  "расхождением больше 1 мм.\nКонфликтных вершин: "
                                  (itoa (length conflicts)) "."))
                              nil)
                            (unresolved
                              (opor-error-circles
                                (opor-slope-level-index-points unresolved points))
                              (opor-alert
                                (strcat
                                  "Расчёт не записан: часть областей не связана с "
                                  "базовой отметкой.\nНерассчитанных вершин: "
                                  (itoa (length unresolved)) "."))
                              nil)
                            (t
                              (setq marks (opor-slope-level-marks))
                              (setq write-result
                                (opor-slope-level-write
                                  points known marks plus-p))
                              (opor-session-set 'slope-level-area-count
                                (length area-records))
                              (opor-session-set 'slope-level-mark-count
                                (length points))
                              (opor-session-set 'slope-level-inserted-count
                                (nth 0 write-result))
                              (opor-session-set 'slope-level-updated-count
                                (nth 1 write-result))
                              (opor-session-set 'slope-level-failed-count
                                (nth 2 write-result))
                              (opor-log
                                (strcat
                                  "Slope levels завершён: областей="
                                  (itoa (length area-records))
                                  ", отметок=" (itoa (length points))
                                  ", добавлено=" (itoa (nth 0 write-result))
                                  ", обновлено=" (itoa (nth 1 write-result))
                                  ", ошибок записи=" (itoa (nth 2 write-result))
                                  "."))
                              (opor-alert
                                (strcat
                                  "Высотные отметки рассчитаны.\nОбластей: "
                                  (itoa (length area-records))
                                  "\nДобавлено отметок: " (itoa (nth 0 write-result))
                                  "\nОбновлено отметок: " (itoa (nth 1 write-result))))
                              (= (nth 2 write-result) 0)))))))))))))))))

(defun opor-command-slope-levels ()
  (opor-init-session)
  (opor-slope-level-run))

(princ)
