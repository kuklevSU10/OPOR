;;; OPOR GEO: импорт высот из proxy-точек геодезической сетки.
;;; Исходные ACAD_PROXY_ENTITY не изменяются: команда копирует их на месте,
;;; раскрывает только временные копии, читает XYZ из полученных окружностей и
;;; удаляет всю временную геометрию до вставки отметок OPOR.

(setq *opor-geo-temp-entities* '())

(defun opor-geo-temp-add (en)
  (if (and en (= (type en) 'ENAME))
    (setq *opor-geo-temp-entities* (cons en *opor-geo-temp-entities*)))
  en)

(defun opor-geo-temp-cleanup (/ count en)
  (setq count 0)
  (foreach en *opor-geo-temp-entities*
    (if (and en (= (type en) 'ENAME) (entget en))
      (progn
        (entdel en)
        (setq count (1+ count)))))
  (setq *opor-geo-temp-entities* '())
  count)

(defun opor-geo-drawing-unit-scale (boundary / bbox diagonal)
  (setq bbox (opor-bbox boundary)
        diagonal (opor-bbox-diagonal bbox))
  (if (< diagonal *opor-geo-meter-drawing-threshold*) 0.001 1.0))

(defun opor-geo-default-step (boundary)
  (if (< (opor-bbox-diagonal (opor-bbox boundary))
         *opor-geo-meter-drawing-threshold*)
    *opor-geo-default-step-m*
    *opor-geo-default-step-mm*))

(defun opor-geo-pick-boundary (/ picked obj)
  (setq picked (entsel "\nВыберите замкнутый контур области геоотметок: "))
  (if picked
    (progn
      (setq obj (vlax-ename->vla-object (car picked)))
      (cond
        ((not (opor-polyline-object-p obj))
          (opor-alert "Выбранный объект не является полилинией.")
          nil)
        ((not (opor-polyline-closed-p obj))
          (opor-alert "Для геоотметок нужен замкнутый контур.")
          nil)
        (t obj)))
    nil))

(defun opor-geo-boundary-polygon (boundary / start end param points pt last)
  ;; Четыре отсчёта на сегмент сохраняют дуговые участки заметно точнее, чем
  ;; только DXF-вершины, и остаются достаточно лёгкими для ssget _CP.
  (setq start (vlax-curve-getStartParam boundary)
        end (vlax-curve-getEndParam boundary)
        param start
        points '())
  (while (< param end)
    (foreach fraction '(0.0 0.25 0.5 0.75)
      (setq pt
        (vl-catch-all-apply
          'vlax-curve-getPointAtParam
          (list boundary (min end (+ param fraction)))))
      (if (and (not (vl-catch-all-error-p pt)) pt)
        (setq points (cons (opor-2d pt) points))))
    (setq param (+ param 1.0)))
  (setq points (reverse points))
  (if (> (length points) 2)
    (progn
      (setq last (opor-list-tail points))
      (if (equal (car points) last 1e-9)
        (setq points (reverse (cdr (reverse points)))))))
  points)

(defun opor-geo-point-on-segment-p (pt a b tol / ab ap cross dot len2)
  (setq ab (list (- (car b) (car a)) (- (cadr b) (cadr a)) 0.0)
        ap (list (- (car pt) (car a)) (- (cadr pt) (cadr a)) 0.0)
        cross (abs (- (* (car ab) (cadr ap)) (* (cadr ab) (car ap))))
        dot (+ (* (car ap) (car ab)) (* (cadr ap) (cadr ab)))
        len2 (+ (* (car ab) (car ab)) (* (cadr ab) (cadr ab))))
  (and (<= cross (* tol (max 1.0 (sqrt len2))))
       (>= dot (- tol))
       (<= dot (+ len2 tol))))

(defun opor-geo-point-in-polygon-p (pt polygon tol / inside edge a b dy xint)
  (setq inside nil edge nil a (opor-list-tail polygon))
  (foreach b polygon
    (cond
      ((opor-geo-point-on-segment-p pt a b tol)
        (setq edge T))
      ((or
         (and (> (cadr a) (cadr pt)) (<= (cadr b) (cadr pt)))
         (and (> (cadr b) (cadr pt)) (<= (cadr a) (cadr pt))))
        (setq dy (- (cadr b) (cadr a)))
        (if (/= dy 0.0)
          (progn
            (setq xint
              (+ (car a)
                 (/ (* (- (cadr pt) (cadr a)) (- (car b) (car a))) dy)))
            (if (< (car pt) xint)
              (setq inside (not inside)))))))
    (setq a b))
  (if edge T inside))

(defun opor-geo-select-proxies (boundary polygon / result bbox raw)
  (setq result
    (vl-catch-all-apply
      'ssget
      (list
        "_CP"
        polygon
        (list
          (cons 0 "ACAD_PROXY_ENTITY")
          (cons 8 *opor-geo-source-layer*)
          (cons 410 "Model")))))
  (if (or (vl-catch-all-error-p result) (not result))
    (progn
      (setq bbox (opor-bbox boundary))
      (if bbox
        (setq raw
          (vl-catch-all-apply
            'ssget
            (list
              "_C" (car bbox) (cadr bbox)
              (list
                (cons 0 "ACAD_PROXY_ENTITY")
                (cons 8 *opor-geo-source-layer*)
                (cons 410 "Model"))))))
      (if (vl-catch-all-error-p raw) nil raw))
    result))

(defun opor-geo-entities-after (anchor / result en)
  (setq result '() en (if anchor (entnext anchor) (entnext)))
  (while en
    (setq result (cons en result)
          en (entnext en)))
  (reverse result))

(defun opor-geo-copy-proxies (selection / before result entities copies en)
  (setq before (entlast))
  (setq result
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "_.COPY" selection ""
        "_non" '(0.0 0.0 0.0)
        "_non" '(0.0 0.0 0.0))))
  (if (vl-catch-all-error-p result)
    nil
    (progn
      (setq entities (opor-geo-entities-after before) copies '())
      (foreach en entities
        (if (= (cdr (assoc 0 (entget en))) "ACAD_PROXY_ENTITY")
          (progn
            (opor-geo-temp-add en)
            (setq copies (cons en copies)))))
      (reverse copies))))

(defun opor-geo-explode-copies (copies anchor / total done failed en result)
  (setq total (length copies) done 0 failed 0)
  (foreach en copies
    (setq result
      (vl-catch-all-apply 'vl-cmdf (list "_.EXPLODE" en)))
    (if (or (vl-catch-all-error-p result) (entget en))
      (setq failed (1+ failed)))
    (setq done (1+ done))
    (if (= 0 (rem done 1000))
      (princ
        (strcat
          "\n[OPOR] Геоточки: обработано " (itoa done)
          " из " (itoa total) "..."))))
  failed)

(defun opor-geo-read-exploded (anchor / entities en ed circles mtexts symbols points)
  (setq entities (opor-geo-entities-after anchor)
        circles 0 mtexts 0 symbols 0 points '())
  (foreach en entities
    (opor-geo-temp-add en)
    (setq ed (entget en))
    (cond
      ((= (cdr (assoc 0 ed)) "CIRCLE")
        (setq circles (1+ circles))
        (if (assoc 10 ed)
          (setq points (cons (cdr (assoc 10 ed)) points))))
      ((= (cdr (assoc 0 ed)) "MTEXT")
        (setq mtexts (1+ mtexts)))
      ((= (cdr (assoc 0 ed)) "LWPOLYLINE")
        (setq symbols (1+ symbols)))))
  (list
    (cons 'points (reverse points))
    (cons 'circles circles)
    (cons 'mtexts mtexts)
    (cons 'symbols symbols)))

(defun opor-geo-extract-points (selection / expected copies anchor failed data)
  (setq *opor-geo-temp-entities* '()
        expected (sslength selection)
        copies (opor-geo-copy-proxies selection))
  (cond
    ((not copies)
      (list (cons 'error "Не удалось создать временные копии геоточек.")))
    ((/= (length copies) expected)
      (list
        (cons 'error
          (strcat
            "Скопировано proxy " (itoa (length copies))
            " из " (itoa expected) "."))))
    (t
      (setq anchor
        (entmakex
          (list
            (cons 0 "POINT")
            (cons 8 "0")
            (cons 10 '(0.0 0.0 0.0))
            (cons 60 1))))
      (opor-geo-temp-add anchor)
      (setq failed (opor-geo-explode-copies copies anchor)
            data (opor-geo-read-exploded anchor))
      (if (or (> failed 0)
              (/= (cdr (assoc 'circles data)) expected))
        (list
          (cons 'error
            (strcat
              "Формат геоточек прочитан не полностью: proxy=" (itoa expected)
              ", окружностей=" (itoa (cdr (assoc 'circles data)))
              ", ошибок EXPLODE=" (itoa failed) "."))
          (cons 'details data))
        data))))

(defun opor-geo-point-key (pt tol)
  (list
    (opor-round (/ (car pt) tol))
    (opor-round (/ (cadr pt) tol))))

(defun opor-geo-key-less-p (a b tol / ka kb)
  (setq ka (opor-geo-point-key a tol)
        kb (opor-geo-point-key b tol))
  (or (< (car ka) (car kb))
      (and (= (car ka) (car kb))
           (< (cadr ka) (cadr kb)))))

(defun opor-geo-finish-group (group result conflicts duplicate-count height-tol / first sum count minz maxz p)
  (if group
    (progn
      (setq first (car group) sum 0.0 count 0 minz nil maxz nil)
      (foreach p group
        (setq sum (+ sum (caddr p))
              count (1+ count)
              minz (if minz (min minz (caddr p)) (caddr p))
              maxz (if maxz (max maxz (caddr p)) (caddr p))))
      (if (> (- maxz minz) height-tol)
        ;; Две поверхности в одном XY нельзя усреднять: получится физически
        ;; несуществующая отметка. Плотная сетка позволяет безопасно пропустить
        ;; такой узел и интерполировать соседние целевые точки вокруг него.
        (setq conflicts (1+ conflicts))
        (setq result
          (cons
            (list (car first) (cadr first) (/ sum count))
            result)))
      (setq duplicate-count (+ duplicate-count (1- count)))))
  (list result conflicts duplicate-count))

(defun opor-geo-dedupe-points (points tol / sorted current-key group result conflicts duplicates p key closed)
  (setq sorted
    (vl-sort points
      '(lambda (a b) (opor-geo-key-less-p a b tol))))
  (setq current-key nil group '() result '() conflicts 0 duplicates 0)
  (foreach p sorted
    (setq key (opor-geo-point-key p tol))
    (if (and current-key (not (equal key current-key)))
      (progn
        (setq closed
          (opor-geo-finish-group
            group result conflicts duplicates (* tol 5.0)))
        (setq result (nth 0 closed)
              conflicts (nth 1 closed)
              duplicates (nth 2 closed)
              group '())))
    (setq current-key key group (cons p group)))
  (setq closed
    (opor-geo-finish-group
      group result conflicts duplicates (* tol 5.0)))
  (list (reverse (nth 0 closed)) (nth 1 closed) (nth 2 closed)))

(defun opor-geo-grid-targets (points step / minx miny cells p key cx cy d2 old result)
  (foreach p points
    (setq minx (if minx (min minx (car p)) (car p))
          miny (if miny (min miny (cadr p)) (cadr p))))
  (setq cells '())
  (foreach p points
    (setq key
      (list
        (fix (/ (- (car p) minx) step))
        (fix (/ (- (cadr p) miny) step)))
      cx (+ minx (* (+ (car key) 0.5) step))
      cy (+ miny (* (+ (cadr key) 0.5) step))
      d2 (+ (* (- (car p) cx) (- (car p) cx))
            (* (- (cadr p) cy) (- (cadr p) cy)))
      old (assoc key cells))
    (if old
      (if (< d2 (caddr old))
        (setq cells (subst (list key p d2) old cells)))
      (setq cells (cons (list key p d2) cells))))
  (setq result (mapcar 'cadr cells))
  (vl-sort result
    '(lambda (a b)
       (or (< (cadr a) (cadr b))
           (and (= (cadr a) (cadr b)) (< (car a) (car b)))))))

(defun opor-geo-nearest-distance (pt points tol / best p d)
  (foreach p points
    (setq d (distance (opor-2d pt) (opor-2d p)))
    (if (and (> d tol) (or (null best) (< d best)))
      (setq best d)))
  best)

(defun opor-geo-source-spacing (points tol / sample distances rest d sorted n)
  (setq sample points distances '() n 0)
  (while (and sample (< n 40))
    (setq d (opor-geo-nearest-distance (car sample) points tol))
    (if d (setq distances (cons d distances)))
    (setq sample (cdr sample) n (1+ n)))
  (if distances
    (progn
      (setq sorted (vl-sort distances '<))
      (nth (fix (/ (length sorted) 2)) sorted))
    nil))

(defun opor-geo-curve-points (obj / name)
  (setq name (opor-obj-name obj))
  (cond
    ((opor-polyline-object-p obj) (opor-polyline-vertices obj))
    ((= name "AcDbLine") (list (opor-curve-start obj) (opor-curve-end obj)))
    (t '())))

(defun opor-geo-select-target-points (boundary tol / ss idx obj points)
  (princ
    "\nВыберите линии/полилинии для отметок или Enter — только вершины контура: ")
  (setq ss (ssget (list (cons 0 "LINE,LWPOLYLINE,POLYLINE"))))
  (if ss
    (progn
      (setq idx 0 points '())
      (while (< idx (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss idx))
              points (append points (opor-geo-curve-points obj))
              idx (1+ idx)))
      (opor-unique-points points tol))
    (opor-unique-points (opor-polyline-vertices boundary) tol)))

(defun opor-geo-nearest-records (target points limit / records p dx dy d2)
  (setq records '())
  (foreach p points
    (setq dx (- (car p) (car target))
          dy (- (cadr p) (cadr target))
          d2 (+ (* dx dx) (* dy dy))
          records (cons (cons d2 p) records)
          records (vl-sort records '(lambda (a b) (< (car a) (car b)))))
    (if (> (length records) limit)
      (setq records (reverse (cdr (reverse records))))))
  records)

(defun opor-geo-orient2d (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a)))))

(defun opor-geo-nearest-triangle (records tol / a tail1 tail2 b c tri)
  (setq a (cdr (car records)) tail1 (cdr records) tri nil)
  (while (and tail1 (not tri))
    (setq b (cdr (car tail1)) tail2 (cdr tail1))
    (while (and tail2 (not tri))
      (setq c (cdr (car tail2)))
      (if (> (abs (opor-geo-orient2d a b c)) (* tol tol))
        (setq tri (list a b c)))
      (setq tail2 (cdr tail2)))
    (setq tail1 (cdr tail1)))
  tri)

(defun opor-geo-triangle-z-at-point (pt tri / a b c den l1 l2 l3)
  (setq a (car tri) b (cadr tri) c (caddr tri)
        den
          (+
            (* (- (cadr b) (cadr c)) (- (car a) (car c)))
            (* (- (car c) (car b)) (- (cadr a) (cadr c)))))
  (if (equal den 0.0 1e-12)
    nil
    (progn
      (setq l1
        (/ (+
             (* (- (cadr b) (cadr c)) (- (car pt) (car c)))
             (* (- (car c) (car b)) (- (cadr pt) (cadr c))))
           den)
            l2
        (/ (+
             (* (- (cadr c) (cadr a)) (- (car pt) (car c)))
             (* (- (car a) (car c)) (- (cadr pt) (cadr c))))
           den)
            l3 (- 1.0 l1 l2))
      (+ (* l1 (caddr a)) (* l2 (caddr b)) (* l3 (caddr c))))))

(defun opor-geo-height-at-point (target source spacing tol / records nearest-distance tri value)
  (setq records (opor-geo-nearest-records target source 8))
  (if records
    (progn
      (setq nearest-distance (sqrt (car (car records))))
      (cond
        ((<= nearest-distance (* spacing 0.25))
          (caddr (cdr (car records))))
        ((> nearest-distance (* spacing 3.0)) nil)
        ((setq tri (opor-geo-nearest-triangle records tol))
          (setq value (opor-geo-triangle-z-at-point target tri))
          (if (numberp value) value nil))
        (t nil)))
    nil))

(defun opor-geo-vertex-targets (targets source spacing tol / result unresolved pt height)
  (setq result '() unresolved '())
  (foreach pt targets
    (setq height (opor-geo-height-at-point pt source spacing tol))
    (if (numberp height)
      (setq result
        (cons
          (list (car pt) (cadr pt) height)
          result))
      (setq unresolved (cons (opor-2d pt) unresolved))))
  (list (reverse result) (reverse unresolved)))

(defun opor-geo-mark-template (/ found obj)
  (vlax-for obj (opor-ms)
    (if (and (not found) (opor-level-block-p obj))
      (setq found obj)))
  found)

(defun opor-geo-old-marks (boundary / marks result mark obj)
  (setq marks (opor-level-read-marks boundary) result '())
  (foreach mark marks
    (setq obj (cdr (assoc 'object mark)))
    (if (and obj (= (opor-object-xdata-type obj) "geo-level-mark"))
      (setq result (cons mark result))))
  (reverse result))

(defun opor-geo-manual-marks (boundary / marks result mark obj)
  (setq marks (opor-level-read-marks boundary) result '())
  (foreach mark marks
    (setq obj (cdr (assoc 'object mark)))
    (if (or (not obj) (/= (opor-object-xdata-type obj) "geo-level-mark"))
      (setq result (cons mark result))))
  (reverse result))

(defun opor-geo-mark-near-p (marks pt tol / found mark mp)
  (foreach mark marks
    (if (not found)
      (progn
        (setq mp (cdr (assoc 'point mark)))
        (if (<= (distance (opor-2d pt) (opor-2d mp)) tol)
          (setq found T)))))
  found)

(defun opor-geo-insert-mark (record template default-scale / pt height sx sy sz rotation layer color raw block)
  (setq pt (list (car record) (cadr record) 0.0)
        height (caddr record)
        sx (if template (vla-get-XScaleFactor template) default-scale)
        sy (if template (vla-get-YScaleFactor template) default-scale)
        sz (if template (vla-get-ZScaleFactor template) default-scale)
        rotation (if template (vla-get-Rotation template) 0.0)
        layer (if template (vla-get-Layer template) "0")
        color (if template (vla-get-Color template) 256)
        raw
          (vl-catch-all-apply
            '(lambda ()
               (vla-InsertBlock
                 (opor-ms) (vlax-3d-point pt) *opor-level-block-name*
                 sx sy sz rotation))
            nil))
  (if (vl-catch-all-error-p raw)
    nil
    (progn
      (setq block raw)
      (vl-catch-all-apply 'vla-put-Layer (list block layer))
      (vl-catch-all-apply 'vla-put-Color (list block color))
      (if (opor-tin-set-mark-text block (rtos height 2 3))
        (progn
          (opor-register-created block "geo-level-mark")
          block)
        (progn
          (opor-delete-object block)
          nil)))))

(defun opor-geo-delete-mark-records (marks / count mark obj)
  (setq count 0)
  (foreach mark marks
    (setq obj (cdr (assoc 'object mark)))
    (if (and obj (opor-object-live-p obj))
      (progn
        (opor-unregister-created obj)
        (opor-delete-object obj)
        (setq count (1+ count)))))
  count)

(defun opor-geo-delete-blocks (blocks / block)
  (foreach block blocks
    (if (opor-object-live-p block)
      (progn
        (opor-unregister-created block)
        (opor-delete-object block))))
  (princ))

(defun opor-geo-write-marks (records boundary tol / template scale old manual inserted skipped failed record block)
  (setq template (opor-geo-mark-template)
        scale (opor-geo-drawing-unit-scale boundary)
        old (opor-geo-old-marks boundary)
        manual (opor-geo-manual-marks boundary)
        inserted '() skipped 0 failed 0)
  (foreach record records
    (if (opor-geo-mark-near-p manual record tol)
      (setq skipped (1+ skipped))
      (progn
        (setq block (opor-geo-insert-mark record template scale))
        (if block
          (setq inserted (cons block inserted))
          (setq failed (1+ failed))))))
  (if (> failed 0)
    (progn
      (opor-geo-delete-blocks inserted)
      (list 0 skipped failed 0))
    (list
      (length inserted)
      skipped
      0
      (opor-geo-delete-mark-records old))))

(defun opor-geo-run (/ boundary polygon selection mode step target-points extraction error source filtered tol deduped conflicts duplicates spacing targets vertex-result unresolved write-result removed-temp)
  (opor-view-save)
  (setq boundary (opor-geo-pick-boundary))
  (if (not boundary)
    nil
    (progn
      (setq polygon (opor-geo-boundary-polygon boundary)
            tol (opor-geo-drawing-unit-scale boundary))
      (if (< (length polygon) 3)
        (progn
          (opor-alert "Не удалось прочитать геометрию контура.")
          nil)
        (progn
          (initget "Сетка Линии")
          (setq mode
            (getkword
              "\nКуда поставить отметки [Сетка/Линии] <Сетка>: "))
          (if (not mode) (setq mode "Сетка"))
          (if (= mode "Сетка")
            (progn
              (setq step (getdist
                (strcat
                  "\nШаг отметок <"
                  (rtos (opor-geo-default-step boundary) 2 3)
                  ">: ")))
              (if (not step) (setq step (opor-geo-default-step boundary))))
            (setq target-points
              (opor-geo-select-target-points boundary tol)))
          (cond
            ((and (= mode "Сетка") (<= step 0.0))
              (opor-alert "Шаг должен быть больше нуля.")
              nil)
            ((and (= mode "Линии") (not target-points))
              (opor-alert "Не найдены точки для расстановки отметок.")
              nil)
            ((not (opor-import-level-block))
              (opor-alert
                "Не найден блок otmetka_oporvb и не удалось загрузить его из библиотеки.")
              nil)
            (t
              (setq selection (opor-geo-select-proxies boundary polygon))
              (if (not selection)
                (progn
                  (opor-alert
                    (strcat
                      "Внутри контура не найдены proxy-точки на слое "
                      *opor-geo-source-layer* "."))
                  nil)
                (progn
                  (opor-log
                    (strcat
                      "GEO: найдено proxy=" (itoa (sslength selection))
                      ", начинается безопасное чтение копий."))
                  (setq extraction
                    (vl-catch-all-apply
                      'opor-geo-extract-points
                      (list selection)))
                  (setq removed-temp (opor-geo-temp-cleanup))
                  (if (vl-catch-all-error-p extraction)
                    (setq error (vl-catch-all-error-message extraction))
                    (setq error (cdr (assoc 'error extraction))))
                  (if error
                    (progn
                      (opor-alert
                        (strcat
                          "Не удалось прочитать геоточки.\n" error
                          "\nВременная геометрия удалена: "
                          (itoa removed-temp) "."))
                      nil)
                    (progn
                      (setq source (cdr (assoc 'points extraction))
                            filtered
                              (vl-remove-if-not
                                '(lambda (p)
                                   (opor-geo-point-in-polygon-p p polygon tol))
                                source)
                            deduped (opor-geo-dedupe-points filtered tol)
                            source (nth 0 deduped)
                            conflicts (nth 1 deduped)
                            duplicates (nth 2 deduped)
                            spacing (opor-geo-source-spacing source tol))
                      (cond
                        ((< (length source) 3)
                          (opor-alert "После фильтрации осталось меньше трёх геоточек.")
                          nil)
                        ((not spacing)
                          (opor-alert "Не удалось определить шаг исходной геосетки.")
                          nil)
                        (t
                          (if (= mode "Сетка")
                            (setq targets (opor-geo-grid-targets source step))
                            (progn
                              (setq vertex-result
                                (opor-geo-vertex-targets
                                  target-points source spacing tol)
                                    targets (car vertex-result)
                                    unresolved (cadr vertex-result))))
                          (cond
                            ((> (length targets) *opor-geo-max-created-marks*)
                              (opor-alert
                                (strcat
                                  "Получится слишком много отметок: "
                                  (itoa (length targets)) ".\n"
                                  "Увеличьте шаг или выберите меньше линий."))
                              nil)
                            ((not targets)
                              (opor-alert "Нет точек, в которых можно поставить отметки.")
                              nil)
                            (t
                              (setq write-result
                                (opor-geo-write-marks
                                  targets boundary (* spacing 0.25)))
                              (if (> (nth 2 write-result) 0)
                                (progn
                                  (opor-alert
                                    (strcat
                                      "Отметки не записаны. Ошибок вставки: "
                                      (itoa (nth 2 write-result)) "."))
                                  nil)
                                (progn
                                  (opor-session-set 'geo-source-count (length source))
                                  (opor-session-set 'geo-target-count (length targets))
                                  (opor-session-set 'geo-inserted-count (nth 0 write-result))
                                  (opor-session-set 'geo-skipped-count (nth 1 write-result))
                                  (opor-session-set 'geo-replaced-count (nth 3 write-result))
                                  (opor-log
                                    (strcat
                                      "GEO завершён: source=" (itoa (length source))
                                      ", spacing=" (rtos spacing 2 6)
                                      ", duplicates=" (itoa duplicates)
                                      ", skipped-conflicts=" (itoa conflicts)
                                      ", targets=" (itoa (length targets))
                                      ", inserted=" (itoa (nth 0 write-result))
                                      ", skipped-manual=" (itoa (nth 1 write-result))
                                      ", replaced=" (itoa (nth 3 write-result))
                                      ", unresolved=" (itoa (length unresolved)) "."))
                                  (opor-alert
                                    (strcat
                                      "Геоотметки расставлены.\n"
                                      "Исходных точек: " (itoa (length source))
                                      "\nДобавлено отметок: " (itoa (nth 0 write-result))
                                      (if (> conflicts 0)
                                        (strcat "\nПропущено конфликтных геоточек: "
                                          (itoa conflicts)) "")
                                      (if (> (nth 1 write-result) 0)
                                        (strcat "\nПропущено ручных отметок: "
                                          (itoa (nth 1 write-result))) "")
                                      (if unresolved
                                        (strcat "\nНе рассчитано вершин: "
                                          (itoa (length unresolved))) "")))
                                  T)))))))))))))))))

(defun opor-command-geo-levels ()
  (opor-init-session)
  (opor-geo-run))

(princ)
