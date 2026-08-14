;;; OPOR command-line UI for MVP

(defun opor-ui-select-mode-cmd (/ mode)
  (initget "Const Var Tin Geo Slope Percent SlopeLevels Height WriteLevel AutoLevel Ring Check Clean Exit")
  (setq mode (getkword "\nOPOR режим [Const/Var/Tin/Geo/Slope/Percent/SlopeLevels/Height/WriteLevel/AutoLevel/Ring/Check/Clean/Exit] <Const>: "))
  (cond
    ((or (not mode) (= mode "Const")) "const-height")
    ((= mode "Var") "var-height")
    ((= mode "Tin") "tin")
    ((= mode "Geo") "geo-levels")
    ((= mode "Slope") "slope")
    ((= mode "Percent") "slopewr")
    ((= mode "SlopeLevels") "slope-levels")
    ((= mode "Height") "height-check")
    ((= mode "WriteLevel") "write-level")
    ((= mode "AutoLevel") "auto-level")
    ((= mode "Ring") "ring")
    ((= mode "Check") "check")
    ((= mode "Clean") "clean")
    (t "exit")))

(defun opor-ui-read-real (prompt default / value)
  (setq value (getreal (strcat prompt " <" (rtos default 2 2) ">: ")))
  (if value value default))

;; Шаг сетки, радиус и размер плитки обязаны быть > 0: оригинал отвергал их
;; в форме (ufrm_steps, btn_ok_Click: "If CDbl(tbox_step.value) <= 0"), DCL-путь
;; тоже (opor-dcl-params-accept). В командном вводе проверки не было, и ноль
;; уходил делителем в opor-index-range-for-bbox, а нулевой радиус - в масштаб
;; блока опоры. Переспрашиваем так же, как длина доски в opor-ui-read-board-layout.
(defun opor-ui-read-positive-real (prompt default / value)
  (setq value (opor-ui-read-real prompt default))
  (while (or (not (numberp value)) (<= value 0.0))
    (princ "\nНужно число > 0.")
    (setq value (opor-ui-read-real prompt default)))
  value)

(defun opor-ui-read-line (/ line)
  (initget "3D PRO")
  (setq line (getkword "\nЛинейка опор [3D/PRO] <3D>: "))
  (if line line "3D"))

(defun opor-ui-read-lag-axis (/ axis)
  (initget "vect perp")
  (setq axis (getkword "\nСчитать длину лаг по сетке [vect/perp] <vect>: "))
  (if axis axis "vect"))

(defun opor-ui-read-tile-mode (/ mode)
  (initget "d p")
  (setq mode (getkword "\nПокрытие [d/p] <d>: "))
  (if mode mode "d"))

(defun opor-ui-read-tile-size (/ sx sy)
  (setq sx
    (opor-ui-read-positive-real "\nРазмер плитки вдоль вектора, мм"
      (cdr (assoc 'tile-size-x *opor-default-params*))))
  (setq sy
    (opor-ui-read-positive-real "\nРазмер плитки вдоль перпендикуляра, мм"
      (cdr (assoc 'tile-size-y *opor-default-params*))))
  (list sx sy))

;; ТЗ П9: длина доски и схема стыков определяют шаг сдвоенных лаг.
(defun opor-ui-read-board-layout (/ boardlen choice layout step)
  (setq boardlen
    (opor-ui-read-real "\nДлина доски, мм"
      (cdr (assoc 'board-length *opor-default-params*))))
  (while (<= boardlen 0.0)
    (princ "\nНужно число > 0.")
    (setq boardlen (opor-ui-read-real "\nДлина доски, мм" 3000.0)))
  (initget "Нет Ровно Половина")
  (setq choice
    (getkword "\nРаскладка доски [Нет/Ровно/Половина] <Нет>: "))
  (setq layout
    (cond
      ((= choice "Ровно") "even")
      ((= choice "Половина") "half")
      (t "none")))
  (setq step
    (cond
      ((= layout "even") boardlen)
      ((= layout "half") (/ boardlen 2.0))
      (t 0.0)))
  (list boardlen layout step))

;; ТЗ П4 (крепёж): лага — по числу опор; плитка — пока по шагу
;; (режим "по формату плитки" появится вместе с портом раскладки плитки)
(defun opor-ui-read-fasteners (line tile-mode / suffix lagk tilek step lagf tilef err)
  (setq suffix (if (= line "3D") " 3D" ""))
  (initget "TOP Clip Нет")
  (setq lagk (getkword "\nКрепление лаги [TOP/Clip/Нет] <Нет>: "))
  (if (or (not lagk) (= lagk "Нет"))
    (setq lagf nil)
    (setq lagf
      (strcat "Level " (if (= lagk "TOP") "TOP" "Clip") suffix)))
  (setq tilef nil step nil)
  (if (= tile-mode "p")
    (progn
      (initget "Lastra Tile TileSoft LastraSoft Нет")
      (setq tilek (getkword "\nКрепление плитки [Lastra/Tile/TileSoft/LastraSoft/Нет] <Нет>: "))
      (if (and tilek (/= tilek "Нет"))
        (setq tilef
        (strcat "Level "
          (cond
            ((= tilek "Lastra") "Lastra")
            ((= tilek "Tile") "Tile")
            ((= tilek "TileSoft") "Tile Soft")
            (t "Lastra Soft"))
          suffix)))
      (if (opor-lag-tile-fastener-p tilef)
        (setq step (opor-ui-read-positive-real "\nШаг креплений плитки, мм" 500.0)))))
  (setq err (opor-floor-composition-error-values tile-mode lagf tilef))
  (if err
    (progn
      (opor-alert err)
      nil)
    (progn
      (opor-session-set 'lag-fastener lagf)
      (opor-session-set 'tile-fastener tilef)
      (opor-session-set 'tile-fastener-step step)
      T)))

;; a_main: "Точка начала вне контура" -> отказ (на границе - допустимо)
(defun opor-ui-base-point-ok-p (base / boundary)
  (setq boundary (opor-session-get 'outer-boundary))
  (if (or (not boundary)
          (opor-point-inside-boundary-p base boundary)
          (opor-point-on-curve-p base boundary *opor-point-tolerance*))
    T
    (progn
      (opor-alert "Точка начала вне контура.")
      nil)))

(defun opor-ui-choose-support (line / supports idx pick support)
  (setq supports (opor-read-supports line))
  (if supports
    (progn
      (princ "\nДоступные опоры из таблицы 'Опоры':")
      (foreach support supports
        (princ
          (strcat
            "\n  "
            (itoa (cdr (assoc 'index support)))
            ". "
            (cdr (assoc 'name support))
            "  "
            (cdr (assoc 'range support))
            "  цвет "
            (itoa (cdr (assoc 'color support))))))
      (setq pick (getint "\nНомер опоры <1>: "))
      (if (not pick) (setq pick 1))
      (setq support
        (vl-some
          '(lambda (item)
             (if (= (cdr (assoc 'index item)) pick) item nil))
          supports))
      (if support support (car supports)))
    (progn
      (opor-log "Таблица 'Опоры' не найдена или пуста; будут использованы ручные значения.")
      (setq idx (getstring T "\nНаименование опоры <OPOR>: "))
      (if (= idx "") (setq idx "OPOR"))
      (setq pick (getstring T "\nДиапазон высот/атрибут <- >: "))
      (if (= pick "") (setq pick "-"))
      (list
        (cons 'index 1)
        (cons 'name idx)
        (cons 'range pick)
        (cons 'color (opor-ui-read-real "\nЦвет опоры" 256.0))))))

(defun opor-ui-line-ready-p (line / block table table-ready missing)
  (setq block (opor-support-block-name line))
  (setq table (opor-table-block-name line))
  (setq table-ready
    (or
      (opor-block-exists-p table)
      (and (member line '("3D" "PRO"))
           (opor-new-table-blocks-ready-p))))
  (setq missing '())
  (if (not (opor-block-exists-p block)) (setq missing (cons block missing)))
  (if (not table-ready) (setq missing (cons table missing)))
  (if missing
    (progn
      (opor-alert (strcat "Для выбранной линейки не найдены блоки:" (opor-join-lines (reverse missing))))
      nil)
    T))

;; UX v3.7: подсветка контура на время кликов + снятие из error-handler
(defun opor-ui-highlight (obj on)
  (vl-catch-all-apply 'vla-Highlight (list obj (if on :vlax-true :vlax-false)))
  (vl-catch-all-apply 'vla-Update (list obj)))

(defun opor-unhighlight-session (/ obj)
  (setq obj
    (if (and (boundp '*opor-session*) *opor-session*)
      (opor-session-get 'highlighted-object)
      nil))
  (if obj
    (progn
      (opor-ui-highlight obj nil)
      (opor-session-set 'highlighted-object nil)))
  (princ))

;; UX v3.7: клики точек — контур подсвечен, промах = переспрос (не отмена),
;; Enter = авто-дефолты, Esc = штатное прерывание через *error*.
;; В мультиконтурном цикле базовая точка/направление спрашиваются каждый раз,
;; а точка общей таблицы — только для первого контура.
(defun opor-ui-pick-base-direction (/ boundary defaults base dir done)
  (setq boundary (opor-session-get 'outer-boundary))
  (setq defaults (opor-grid-default-base-dir boundary))
  (opor-ui-highlight boundary T)
  (opor-session-set 'highlighted-object boundary)
  (setq done nil)
  (while (not done)
    (setq base (getpoint "\nНачальная точка сетки <Enter = угол у длинного ребра>: "))
    (cond
      ((not base)
        (setq base (car defaults))
        (setq done T))
      ((or (opor-point-inside-boundary-p (opor-2d base) boundary)
           (opor-point-on-curve-p (opor-2d base) boundary *opor-point-tolerance*))
        (setq base (opor-2d base))
        (setq done T))
      (t
        (princ "\nТочка вне контура — кликните внутри подсвеченного контура или Enter для авто."))))
  (setq dir (getpoint base "\nНаправление сетки <Enter = вдоль длинного ребра>: "))
  (if dir
    (setq dir (opor-2d dir))
    (setq dir (opor-v+ base (opor-v- (cadr defaults) (car defaults)))))
  (opor-ui-highlight boundary nil)
  (opor-session-set 'highlighted-object nil)
  (list base dir))

(defun opor-ui-pick-points (/ pair table-point boundary bbox ll ur dx)
  (setq pair (opor-ui-pick-base-direction))
  (setq boundary (opor-session-get 'outer-boundary))
  (setq table-point (getpoint "\nТочка вставки итоговой таблицы <Enter = справа от контура>: "))
  (if table-point
    (setq table-point (opor-2d table-point))
    (progn
      (setq bbox (opor-bbox boundary))
      (setq ll (car bbox))
      (setq ur (cadr bbox))
      (setq dx (max 1000.0 (* 0.15 (- (car ur) (car ll)))))
      (setq table-point (list (+ (car ur) dx) (cadr ur) 0.0))))
  (list (car pair) (cadr pair) table-point))

(defun opor-ui-read-next-contour-points (/ pair)
  (setq pair (opor-ui-pick-base-direction))
  (if pair
    (progn
      (opor-session-set 'base-point (car pair))
      (opor-session-set 'direction-point (cadr pair))
      T)
    nil))

(defun opor-ui-read-params-cmd (/ line support axis step-x step-y tile-size radius tile-mode board-layout board-width board-thickness lag-width lag-thickness composition fasteners-ok pts)
  (setq line (opor-ui-read-line))
  (if (not (opor-ui-line-ready-p line))
    nil
    (progn
      (setq support (opor-ui-choose-support line))
      (setq step-x (opor-ui-read-positive-real "\nШаг по вектору, мм" (cdr (assoc 'step-x *opor-default-params*))))
      (setq step-y (opor-ui-read-positive-real "\nШаг по перпендикуляру, мм" (cdr (assoc 'step-y *opor-default-params*))))
      (setq radius (opor-ui-read-positive-real "\nРадиус опоры, мм" (cdr (assoc 'radius *opor-default-params*))))
      ;; S4: покрытие выбирается и в Const (VBA-форма общая); плитка форсит дв. лаги = 0
      (setq tile-mode (opor-ui-read-tile-mode))
      (setq tile-size
        (if (= tile-mode "p")
          (opor-ui-read-tile-size)
          (list (cdr (assoc 'tile-size-x *opor-default-params*))
                (cdr (assoc 'tile-size-y *opor-default-params*)))))
      (setq board-layout
        (if (= tile-mode "d")
          (opor-ui-read-board-layout)
          (list (cdr (assoc 'board-length *opor-default-params*)) "none" 0.0)))
      (if (= tile-mode "d")
        (progn
          (setq board-width
            (opor-ui-read-positive-real "\nШирина доски, мм"
              (cdr (assoc 'board-width *opor-default-params*))))
          (setq board-thickness
            (opor-ui-read-positive-real "\nТолщина доски, мм"
              (cdr (assoc 'board-thickness *opor-default-params*)))))
        (progn
          (setq board-width (cdr (assoc 'board-width *opor-default-params*)))
          (setq board-thickness (cdr (assoc 'board-thickness *opor-default-params*)))))
      (setq fasteners-ok (opor-ui-read-fasteners line tile-mode))
      (if fasteners-ok
        (progn
          (setq composition
            (opor-floor-composition-values
              tile-mode
              (opor-session-get 'lag-fastener)
              (opor-session-get 'tile-fastener)))
          (if (member composition '("board-lag" "tile-lag"))
            (progn
              (setq lag-width
                (opor-ui-read-positive-real "\nШирина лаги, мм"
                  (cdr (assoc 'lag-width *opor-default-params*))))
              (setq lag-thickness
                (opor-ui-read-positive-real "\nВысота лаги, мм"
                  (cdr (assoc 'lag-thickness *opor-default-params*)))))
            (progn
              (setq lag-width (cdr (assoc 'lag-width *opor-default-params*)))
              (setq lag-thickness (cdr (assoc 'lag-thickness *opor-default-params*)))))
          (setq axis (opor-ui-read-lag-axis))
          (setq pts (opor-ui-pick-points))))
      (if (and fasteners-ok pts)
        (progn
          (opor-session-set 'line line)
          (opor-session-set 'support-index (cdr (assoc 'index support)))
          (opor-session-set 'support-name (cdr (assoc 'name support)))
          (opor-session-set 'support-range (cdr (assoc 'range support)))
          (opor-session-set 'support-color (fix (cdr (assoc 'color support))))
          (opor-session-set 'step-x step-x)
          (opor-session-set 'step-y step-y)
          (opor-session-set 'tile-size-x (car tile-size))
          (opor-session-set 'tile-size-y (cadr tile-size))
          (opor-session-set 'radius radius)
          (opor-session-set 'tile-mode tile-mode)
          (opor-session-set 'board-width board-width)
          (opor-session-set 'board-thickness board-thickness)
          (opor-session-set 'board-length (car board-layout))
          (opor-session-set 'lag-width lag-width)
          (opor-session-set 'lag-thickness lag-thickness)
          (opor-session-set 'double-lag-layout (cadr board-layout))
          (opor-session-set 'double-lag-step (caddr board-layout))
          (opor-session-set 'lag-axis axis)
          (opor-session-set 'base-point (car pts))
          (opor-session-set 'direction-point (cadr pts))
          (opor-session-set 'table-point (caddr pts))
          T)
        (progn
          (opor-log "Ввод параметров отменён.")
          nil)))))

(defun opor-ui-read-var-params-cmd (/ line axis step-x step-y tile-size radius board-layout tile-mode floor-height board-width board-thickness lag-width lag-thickness tile-thickness composition fasteners-ok pts supports maxmark)
  (setq line (opor-ui-read-line))
  (if (not (opor-ui-line-ready-p line))
    nil
    (progn
      (setq supports (opor-read-supports line))
      (if (not supports)
        (opor-log "Таблица 'Опоры' не найдена или пуста; переменная высота не сможет разложить диапазоны."))
      (setq step-x (opor-ui-read-positive-real "\nШаг по вектору, мм" (cdr (assoc 'step-x *opor-default-params*))))
      (setq step-y (opor-ui-read-positive-real "\nШаг по перпендикуляру, мм" (cdr (assoc 'step-y *opor-default-params*))))
      (setq radius (opor-ui-read-positive-real "\nРадиус опоры, мм" (cdr (assoc 'radius *opor-default-params*))))
      ;; S4: покрытие спрашивается ДО дв. лаг; плитка форсит дв. лаги = 0 (VBA)
      (setq tile-mode (opor-ui-read-tile-mode))
      (setq tile-size
        (if (= tile-mode "p")
          (opor-ui-read-tile-size)
          (list (cdr (assoc 'tile-size-x *opor-default-params*))
                (cdr (assoc 'tile-size-y *opor-default-params*)))))
      (setq board-layout
        (if (= tile-mode "d")
          (opor-ui-read-board-layout)
          (list (cdr (assoc 'board-length *opor-default-params*)) "none" 0.0)))
      (setq floor-height (opor-ui-read-real "\nОтметка чистого пола, мм" (cdr (assoc 'floor-height *opor-default-params*))))
      ;; a_main: maxlev >= CLng(arrfrm(3)) -> отказ
      (setq maxmark (opor-level-max-mark *opor-session*))
      (if (and maxmark (>= maxmark (opor-round-half-even floor-height)))
        (progn
          (opor-alert
            (strcat
              "Максимальная отметка=" (opor-height-text maxmark)
              "\nУровень чистого пола=" (opor-height-text floor-height)
              "\nУровень чистого пола должен быть выше максимальной отметки."))
          (setq floor-height nil)))
      (if (and floor-height (= tile-mode "d"))
        (progn
          (setq board-width (opor-ui-read-positive-real "\nШирина доски, мм" (cdr (assoc 'board-width *opor-default-params*))))
          (setq board-thickness (opor-ui-read-positive-real "\nТолщина доски, мм" (cdr (assoc 'board-thickness *opor-default-params*))))
          (setq tile-thickness (cdr (assoc 'tile-thickness *opor-default-params*)))))
      (if (and floor-height (= tile-mode "p"))
        (progn
          (setq board-width (cdr (assoc 'board-width *opor-default-params*)))
          (setq board-thickness (cdr (assoc 'board-thickness *opor-default-params*)))
          (setq tile-thickness (opor-ui-read-positive-real "\nТолщина плитки, мм" (cdr (assoc 'tile-thickness *opor-default-params*))))))
      (if floor-height
        (progn
          (setq fasteners-ok (opor-ui-read-fasteners line tile-mode))
          (if fasteners-ok
            (progn
              (setq composition
                (opor-floor-composition-values
                  tile-mode
                  (opor-session-get 'lag-fastener)
                  (opor-session-get 'tile-fastener)))
              (if (member composition '("board-lag" "tile-lag"))
                (progn
                  (setq lag-width (opor-ui-read-positive-real "\nШирина лаги, мм" (cdr (assoc 'lag-width *opor-default-params*))))
                  (setq lag-thickness (opor-ui-read-positive-real "\nВысота лаги, мм" (cdr (assoc 'lag-thickness *opor-default-params*)))))
                (progn
                  (setq lag-width (cdr (assoc 'lag-width *opor-default-params*)))
                  (setq lag-thickness (cdr (assoc 'lag-thickness *opor-default-params*)))))
              (setq axis (opor-ui-read-lag-axis))
              (setq pts (opor-ui-pick-points))))))
      (if (and supports floor-height fasteners-ok pts)
        (progn
          (opor-session-set 'mode "var-height")
          (opor-session-set 'line line)
          (opor-session-set 'support-ranges supports)
          (opor-session-set 'step-x step-x)
          (opor-session-set 'step-y step-y)
          (opor-session-set 'tile-size-x (car tile-size))
          (opor-session-set 'tile-size-y (cadr tile-size))
          (opor-session-set 'radius radius)
          (opor-session-set 'board-length (car board-layout))
          (opor-session-set 'double-lag-layout (cadr board-layout))
          (opor-session-set 'double-lag-step (caddr board-layout))
          (opor-session-set 'tile-mode tile-mode)
          (opor-session-set 'floor-height floor-height)
          (opor-session-set 'board-width board-width)
          (opor-session-set 'board-thickness board-thickness)
          (opor-session-set 'lag-width lag-width)
          (opor-session-set 'lag-thickness lag-thickness)
          (opor-session-set 'tile-thickness tile-thickness)
          (opor-session-set 'lag-axis axis)
          (opor-session-set 'base-point (car pts))
          (opor-session-set 'direction-point (cadr pts))
          (opor-session-set 'table-point (caddr pts))
          T)
        (progn
          (opor-log "Ввод параметров переменной высоты отменён.")
          nil)))))

;; ---------- диспетчеры: DCL или командная строка ----------
(defun opor-ui-select-mode (/ mode)
  (if (opor-dcl-ready-p)
    (progn
      (setq mode (opor-dcl-select-mode))
      (if mode mode (opor-ui-select-mode-cmd)))
    (opor-ui-select-mode-cmd)))

(defun opor-ui-read-params ()
  (if (opor-dcl-ready-p)
    (opor-dcl-read-params)
    (opor-ui-read-params-cmd)))

(defun opor-ui-read-var-params ()
  (if (opor-dcl-ready-p)
    (opor-dcl-read-var-params)
    (opor-ui-read-var-params-cmd)))

(princ)
