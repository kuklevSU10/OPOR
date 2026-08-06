;;; OPOR DCL-панель (по образцу VBA-форм ufrm_mode/ufrm_steps/ufrm_H) + удобства

(setq *opor-form-memory* nil)          ; память формы на сессию AutoCAD (аналог arrfrm)
(setq *opor-dcl-var-p* nil)            ; режим текущего показа формы
(setq *opor-dcl-params-result* nil)

(setq *opor-dcl-lag-fasteners* '("Нет" "Level TOP" "Level Clip"))
(setq *opor-dcl-tile-fasteners* '("Нет" "Level Lastra" "Level Tile" "Level Tile Soft" "Level Lastra Soft"))

(defun opor-form-mem-get (key default / pair)
  (setq pair (assoc key *opor-form-memory*))
  (if pair (cdr pair) default))

(defun opor-form-mem-set (key value / pair)
  (setq pair (assoc key *opor-form-memory*))
  (if pair
    (setq *opor-form-memory* (subst (cons key value) pair *opor-form-memory*))
    (setq *opor-form-memory* (cons (cons key value) *opor-form-memory*)))
  value)

(defun opor-num-str (v)
  (cond
    ((not (numberp v)) "")
    ((equal v (float (fix v)) 1e-9) (itoa (fix v)))
    (t (rtos v 2 2))))

;; findfile без пути ищет только в support-путях AutoCAD — папки OPOR там нет,
;; поэтому первым делом строим полный путь от *opor-root* (его резолвит loader)
(defun opor-dcl-file (/ path)
  (cond
    ((and (boundp '*opor-root*) *opor-root*
          (setq path (findfile (strcat *opor-root* "\\opor.dcl"))))
      path)
    ((findfile "opor.dcl"))
    (t nil)))

(defun opor-dcl-ready-p ()
  (and *opor-use-dcl* (opor-dcl-file)))

(defun opor-dcl-load (/ id)
  (setq id (vl-catch-all-apply 'load_dialog (list (opor-dcl-file))))
  (if (or (vl-catch-all-error-p id) (not id) (< id 0))
    (progn (opor-log "DCL не загрузился — командный ввод.") nil)
    id))

;; ---------- меню (3x3 как sayan-форма) ----------
(defun opor-dcl-show-mode-help (/ id result)
  (setq id (opor-dcl-load))
  (if id
    (if (new_dialog "opor_mode_help" id)
      (progn
        (action_tile "accept" "(done_dialog 1)")
        (setq result (start_dialog))
        (unload_dialog id)
        result)
      (progn
        (unload_dialog id)
        (opor-log "Диалог подсказки OPOR не найден.")
        nil))
    nil))

;; Логотип показан обычным тайлом image, а не image_button: кнопке DCL
;; рисует рамку, отключить её нечем, а прозрачную кнопку поверх картинки
;; не положить — тайлы в DCL не перекрываются. Поэтому логотип
;; некликабельный: вид важнее ссылки.
;; Логотип заказчика в главной панели.
;; DCL не умеет растровые картинки, но умеет заливать прямоугольники,
;; поэтому знак разложен на 235 прямоугольников из векторного logo.svg
;; и рисуется через fill_image. Список сгенерирован скриптом, руками
;; не правится: при смене логотипа нужно перегенерировать целиком.
;; Опорный размер растра — 168 x 49, в панели масштабируется
;; пропорционально под фактический размер тайла.
(setq *opor-logo-ref-width* 168.0)
(setq *opor-logo-ref-height* 49.0)
(setq *opor-logo-rects*
  '(
    (24 0 2 1) (41 0 3 1) (58 0 3 1) (21 1 6 1) (38 1 6 1) (55 1 6 1)
    (18 2 6 1) (25 2 2 3) (35 2 6 1) (42 2 3 3) (52 2 6 1) (59 2 3 14)
    (16 3 5 1) (33 3 5 1) (50 3 6 1) (13 4 5 1) (30 4 6 1) (47 4 6 1)
    (10 5 6 1) (25 5 8 1) (42 5 8 1) (8 6 5 1) (24 6 6 1) (41 6 6 1)
    (5 7 5 1) (22 7 6 1) (39 7 6 1) (2 8 6 1) (19 8 8 1) (36 8 9 1)
    (0 9 5 1) (17 9 5 1) (25 9 2 7) (34 9 5 1) (43 9 2 7) (0 10 2 14)
    (17 10 2 8) (34 10 3 1) (34 11 2 7) (74 15 12 1) (93 15 10 1)
    (112 15 3 1) (124 15 3 1) (134 15 10 1) (154 15 10 1) (23 16 4 1)
    (41 16 3 1) (58 16 3 1) (73 16 13 1) (93 16 12 1) (112 16 4 7)
    (123 16 4 7) (133 16 13 1) (153 16 13 1) (21 17 5 1) (38 17 5 1)
    (55 17 6 1) (72 17 14 1) (93 17 13 1) (133 17 14 1) (153 17 14 1)
    (17 18 6 1) (34 18 7 1) (52 18 6 1) (72 18 4 1) (102 18 4 1)
    (143 18 4 1) (153 18 4 14) (163 18 5 1) (15 19 6 1) (32 19 6 1)
    (50 19 6 1) (72 19 3 1) (103 19 3 1) (144 19 3 1) (164 19 4 13)
    (12 20 11 1) (30 20 10 1) (47 20 11 1) (72 20 4 1) (103 20 4 1)
    (144 20 4 1) (10 21 6 1) (17 21 2 3) (20 21 13 1) (34 21 2 3)
    (38 21 5 1) (44 21 6 1) (55 21 6 1) (72 21 6 1) (95 21 12 1)
    (135 21 13 1) (7 22 6 1) (23 22 7 1) (40 22 7 1) (57 22 5 1) (73 22 8 1)
    (93 22 14 1) (134 22 14 1) (4 23 6 1) (22 23 6 1) (39 23 6 1)
    (56 23 6 1) (74 23 10 1) (92 23 15 1) (112 23 15 1) (133 23 15 1)
    (0 24 7 1) (17 24 10 1) (34 24 8 1) (43 24 2 3) (53 24 5 1) (60 24 2 14)
    (77 24 8 1) (92 24 4 1) (102 24 5 5) (113 24 14 1) (132 24 5 1)
    (144 24 4 5) (0 25 4 1) (16 25 6 1) (25 25 2 2) (33 25 6 1) (50 25 6 1)
    (81 25 5 1) (91 25 4 3) (114 25 13 1) (132 25 4 3) (0 26 5 1)
    (13 26 10 1) (30 26 9 1) (48 26 5 1) (82 26 4 1) (123 26 4 2) (3 27 5 1)
    (11 27 5 1) (20 27 14 1) (37 27 13 1) (83 27 3 1) (5 28 9 1) (23 28 8 1)
    (39 28 9 1) (82 28 4 1) (91 28 5 1) (122 28 5 1) (132 28 5 1) (5 29 6 1)
    (22 29 6 1) (40 29 5 1) (72 29 14 1) (92 29 14 2) (113 29 13 2)
    (133 29 14 2) (3 30 5 1) (19 30 8 1) (37 30 8 1) (72 30 13 1) (1 31 5 1)
    (18 31 4 1) (25 31 2 7) (35 31 4 1) (43 31 2 7) (72 31 12 1)
    (94 31 11 1) (113 31 10 1) (135 31 11 1) (0 32 3 1) (17 32 3 1)
    (34 32 3 1) (0 33 2 13) (17 33 2 7) (34 33 2 7) (24 38 3 1) (41 38 4 1)
    (58 38 4 1) (21 39 6 1) (38 39 6 1) (55 39 6 1) (132 39 5 1)
    (140 39 3 1) (145 39 6 1) (153 39 2 4) (158 39 1 5) (162 39 5 1)
    (17 40 7 1) (34 40 7 1) (53 40 5 1) (131 40 2 4) (136 40 1 4)
    (140 40 1 1) (145 40 2 1) (149 40 2 1) (161 40 2 4) (166 40 1 1)
    (16 41 6 1) (33 41 6 1) (50 41 5 1) (139 41 2 4) (145 41 1 1)
    (150 41 2 2) (166 41 2 2) (13 42 6 1) (30 42 6 1) (47 42 6 1)
    (144 42 2 1) (10 43 6 1) (17 43 2 3) (28 43 5 1) (34 43 2 3) (44 43 6 1)
    (145 43 1 1) (150 43 1 1) (154 43 1 1) (166 43 1 1) (8 44 5 1)
    (25 44 5 1) (42 44 5 1) (132 44 5 1) (145 44 6 1) (154 44 5 1)
    (161 44 6 1) (5 45 6 1) (22 45 5 1) (39 45 6 1) (136 45 1 1)
    (161 45 2 2) (0 46 8 1) (17 46 8 1) (35 46 7 1) (132 46 5 1) (0 47 5 1)
    (18 47 4 1) (35 47 4 1) (162 47 1 1) (2 48 1 1)
   ))

;; Знак рисуем по центру тайла с сохранением пропорций: тайл в разных
;; сборках AutoCAD получается разного размера в пикселях.
(defun opor-dcl-draw-logo (/ w h s ox oy rx ry rw rh)
  (setq w (dimx_tile "logo")
        h (dimy_tile "logo"))
  (if (and w h (> w 8) (> h 8))
    (progn
      (start_image "logo")
      (fill_image 0 0 w h -15)
      (setq s (min (/ (float w) *opor-logo-ref-width*)
                   (/ (float h) *opor-logo-ref-height*)))
      (setq ox (/ (- w (* *opor-logo-ref-width* s)) 2.0)
            oy (/ (- h (* *opor-logo-ref-height* s)) 2.0))
      (foreach r *opor-logo-rects*
        (setq rx (fix (+ ox (* (car r) s)))
              ry (fix (+ oy (* (cadr r) s)))
              rw (fix (+ 0.5 (* (caddr r) s)))
              rh (fix (+ 0.5 (* (cadddr r) s))))
        (if (< rw 1) (setq rw 1))
        (if (< rh 1) (setq rh 1))
        (fill_image rx ry rw rh -16))
      (end_image))))

(defun opor-dcl-select-mode (/ id result)
  (setq id (opor-dcl-load))
  (if (not id)
    nil
    (if (not (new_dialog "opor_mode" id))
      (progn (unload_dialog id) nil)
      (progn
        (set_tile "ver" (strcat "Ver " *opor-version*))
        ;; Размеры тайла известны только у уже созданного диалога,
        ;; поэтому знак рисуем после new_dialog и до start_dialog.
        (vl-catch-all-apply 'opor-dcl-draw-logo)
        ;; VBA-форма: h = write_levl, +0.000 = auto_levl, ? = check_height.
        (action_tile "const" "(done_dialog 2)")
        (action_tile "var"   "(done_dialog 3)")
        (action_tile "clean" "(done_dialog 4)")
        (action_tile "check" "(done_dialog 5)")
        (action_tile "slope" "(done_dialog 6)")
        (action_tile "slopewr" "(done_dialog 7)")
        (action_tile "chkh" "(done_dialog 8)")
        (action_tile "wrlevl" "(done_dialog 9)")
        (action_tile "ring" "(done_dialog 10)")
        (action_tile "tin" "(done_dialog 11)")
        (action_tile "geo" "(done_dialog 13)")
        (action_tile "info" "(done_dialog 12)")
        (action_tile "cancel" "(done_dialog 0)")
        (setq result (start_dialog))
        (unload_dialog id)
        (cond
          ((= result 2) "const-height")
          ((= result 3) "var-height")
          ((= result 4) "clean")
          ((= result 5) "height-check")
          ((= result 6) "slope")
          ((= result 7) "slopewr")
          ((= result 8) "write-level")
          ((= result 9) "auto-level")
          ((= result 10) "ring")
          ((= result 11) "tin")
          ((= result 13) "geo-levels")
          ((= result 12)
            (opor-dcl-show-mode-help)
            (opor-dcl-select-mode))
          (t "exit"))))))

;; ---------- форма «Ввод» ----------
(defun opor-dcl-params-sync (/ var-p cov-d flag-idx ftile-idx tile-f direct-p lag-input-p)
  (setq var-p *opor-dcl-var-p*)
  (setq cov-d (= (get_tile "cov_d") "1"))
  ;; Доска не использует плиточный крепёж. LASTRA, наоборот, означает
  ;; прямую плитку без лаг и автоматически сбрасывает крепление лаг.
  (if cov-d (set_tile "ftile" "0"))
  (setq ftile-idx (atoi (get_tile "ftile")))
  (setq direct-p (and (not cov-d) (member ftile-idx '(1 4))))
  (if direct-p (set_tile "flag" "0"))
  (setq flag-idx (atoi (get_tile "flag")))
  ;; В legacy-плитке без крепежа лаговая сетка сохраняется ради совместимости,
  ;; но её толщина в старую формулу высоты не входила — поле должно быть серым.
  (setq lag-input-p
    (or cov-d
        (and (not direct-p)
             (or (> flag-idx 0) (member ftile-idx '(2 3))))))
  (setq tile-f (and (not cov-d) (member ftile-idx '(2 3))))
  (mode_tile "floor" (if var-p 0 1))
  (mode_tile "cov_d" 0)                  ; S4: покрытие выбирается и в Const (VBA-форма общая)
  (mode_tile "cov_p" 0)
  (mode_tile "doska" (if (and var-p cov-d) 0 1))
  (mode_tile "lag" (if (and var-p lag-input-p) 0 1))
  (mode_tile "plitka" (if (and var-p (not cov-d)) 0 1))
  (mode_tile "tilex" (if (not cov-d) 0 1))
  (mode_tile "tiley" (if (not cov-d) 0 1))
  (mode_tile "boardlen" (if cov-d 0 1))
  (mode_tile "boardlayout" (if cov-d 0 1))
  (mode_tile "tri" (if var-p 0 1))
  (mode_tile "flag" (if direct-p 1 0))
  (mode_tile "ftile" (if cov-d 1 0))
  (mode_tile "fstep" (if tile-f 0 1)))

(defun opor-dcl-cover-click (which)
  (set_tile "cov_d" (if (= which "d") "1" "0"))
  (set_tile "cov_p" (if (= which "p") "1" "0"))
  (opor-dcl-params-sync))

(defun opor-dcl-params-accept (/ sx sy tx ty r fl dsk lg plt fs boardlen layout-idx layout dbl line axis cov flag-idx ftile-idx lag-fastener tile-fastener composition composition-error err)
  (setq sx (distof (get_tile "stepx") 2))
  (setq sy (distof (get_tile "stepy") 2))
  (setq tx (distof (get_tile "tilex") 2))
  (setq ty (distof (get_tile "tiley") 2))
  (setq r (distof (get_tile "radius") 2))
  (setq fl (distof (get_tile "floor") 2))
  (setq dsk (distof (get_tile "doska") 2))
  (setq lg (distof (get_tile "lag") 2))
  (setq plt (distof (get_tile "plitka") 2))
  (setq fs (distof (get_tile "fstep") 2))
  (setq boardlen (distof (get_tile "boardlen") 2))
  (setq layout-idx (atoi (get_tile "boardlayout")))
  (setq layout
    (cond
      ((= layout-idx 1) "even")
      ((= layout-idx 2) "half")
      (t "none")))
  (setq flag-idx (atoi (get_tile "flag")))
  (setq ftile-idx (atoi (get_tile "ftile")))
  (setq line
    (cond
      ((= (get_tile "ln_3d") "1") "3D")
      ((= (get_tile "ln_pro") "1") "PRO")
      (t "3D")))
  (setq axis (if (= (get_tile "ax_perp") "1") "perp" "vect"))
  (setq cov (if (= (get_tile "cov_p") "1") "p" "d"))
  (setq lag-fastener
    (opor-dcl-fastener-name flag-idx *opor-dcl-lag-fasteners* line))
  (setq tile-fastener
    (opor-dcl-fastener-name ftile-idx *opor-dcl-tile-fasteners* line))
  (setq composition
    (opor-floor-composition-values cov lag-fastener tile-fastener))
  (setq composition-error
    (opor-floor-composition-error-values cov lag-fastener tile-fastener))
  (if (= cov "p") (setq layout "none"))
  (setq dbl
    (cond
      ((and (= layout "even") (numberp boardlen)) boardlen)
      ((and (= layout "half") (numberp boardlen)) (/ boardlen 2.0))
      (t 0.0)))
  (setq err
    (cond
      ((or (not sx) (<= sx 0.0)) "Шаг вдоль вектора: нужно число > 0")
      ((or (not sy) (<= sy 0.0)) "Шаг вдоль перпендикуляра: нужно число > 0")
      ((and (= cov "p") (or (not tx) (<= tx 0.0))) "Плитка вдоль вектора: нужно число > 0")
      ((and (= cov "p") (or (not ty) (<= ty 0.0))) "Плитка вдоль перпендикуляра: нужно число > 0")
      ((or (not r) (<= r 0.0)) "Радиус: нужно число > 0")
      ((and (= cov "d") (or (not boardlen) (<= boardlen 0.0))) "Длина доски: нужно число > 0")
      ((and *opor-dcl-var-p* (not fl)) "Отметка чистого пола: нужно число")
      ((and *opor-dcl-var-p* (= cov "d") (or (not dsk) (<= dsk 0.0))) "Толщина доски: нужно число > 0")
      ((and *opor-dcl-var-p*
            (member composition '("board-lag" "tile-lag"))
            (or (not lg) (<= lg 0.0))) "Толщина лаги: нужно число > 0")
      ((and *opor-dcl-var-p* (= cov "p") (or (not plt) (<= plt 0.0))) "Толщина плитки: нужно число > 0")
      (composition-error composition-error)
      ((and (member ftile-idx '(2 3)) (or (not fs) (<= fs 0.0))) "Шаг креплений: нужно число > 0")
      (t nil)))
  (if err
    (set_tile "error" err)
    (progn
      (setq *opor-dcl-params-result*
        (list
          (cons 'line line)
          (cons 'step-x sx)
          (cons 'step-y sy)
          (cons 'tile-size-x (if tx tx (cdr (assoc 'tile-size-x *opor-default-params*))))
          (cons 'tile-size-y (if ty ty (cdr (assoc 'tile-size-y *opor-default-params*))))
          (cons 'radius r)
          (cons 'board-length (if boardlen boardlen (cdr (assoc 'board-length *opor-default-params*))))
          (cons 'double-lag-layout layout)
          (cons 'double-lag-step dbl)
          (cons 'lag-axis axis)
          (cons 'tile-mode cov)
          (cons 'floor-height fl)
          (cons 'board-thickness (if dsk dsk (cdr (assoc 'board-thickness *opor-default-params*))))
          (cons 'lag-thickness (if lg lg (cdr (assoc 'lag-thickness *opor-default-params*))))
          (cons 'tile-thickness (if plt plt (cdr (assoc 'tile-thickness *opor-default-params*))))
          (cons 'lag-fastener-idx flag-idx)
          (cons 'tile-fastener-idx ftile-idx)
          (cons 'tile-fastener-step fs)
          (cons 'show-triangles (= (get_tile "tri") "1"))))
      (foreach pair *opor-dcl-params-result*
        (opor-form-mem-set (car pair) (cdr pair)))
      (done_dialog 1))))

(defun opor-dcl-show-params (mode / id res)
  (setq *opor-dcl-var-p* (= mode "var-height"))
  (setq *opor-dcl-params-result* nil)
  (setq id (opor-dcl-load))
  (if (not id)
    nil
    (if (not (new_dialog "opor_params" id))
      (progn (unload_dialog id) nil)
      (progn
        (set_tile "stepx" (opor-num-str (opor-form-mem-get 'step-x (cdr (assoc 'step-x *opor-default-params*)))))
        (set_tile "stepy" (opor-num-str (opor-form-mem-get 'step-y (cdr (assoc 'step-y *opor-default-params*)))))
        (set_tile "tilex" (opor-num-str (opor-form-mem-get 'tile-size-x (cdr (assoc 'tile-size-x *opor-default-params*)))))
        (set_tile "tiley" (opor-num-str (opor-form-mem-get 'tile-size-y (cdr (assoc 'tile-size-y *opor-default-params*)))))
        (set_tile "radius" (opor-num-str (opor-form-mem-get 'radius (cdr (assoc 'radius *opor-default-params*)))))
        (set_tile "floor" (opor-num-str (opor-form-mem-get 'floor-height (cdr (assoc 'floor-height *opor-default-params*)))))
        (set_tile "doska" (opor-num-str (opor-form-mem-get 'board-thickness (cdr (assoc 'board-thickness *opor-default-params*)))))
        (set_tile "lag" (opor-num-str (opor-form-mem-get 'lag-thickness (cdr (assoc 'lag-thickness *opor-default-params*)))))
        (set_tile "plitka" (opor-num-str (opor-form-mem-get 'tile-thickness (cdr (assoc 'tile-thickness *opor-default-params*)))))
        (set_tile "boardlen" (opor-num-str (opor-form-mem-get 'board-length (cdr (assoc 'board-length *opor-default-params*)))))
        (start_list "boardlayout")
        (foreach item '("Нет" "Ровно" "Сдвиг 1/2") (add_list item))
        (end_list)
        (set_tile "boardlayout"
          (cond
            ((= (opor-form-mem-get 'double-lag-layout "none") "even") "1")
            ((= (opor-form-mem-get 'double-lag-layout "none") "half") "2")
            (t "0")))
        (set_tile (if (= (opor-form-mem-get 'lag-axis "vect") "perp") "ax_perp" "ax_vect") "1")
        (set_tile (if (= (opor-form-mem-get 'tile-mode "d") "p") "cov_p" "cov_d") "1")
        (set_tile
          (if (= (opor-form-mem-get 'line "3D") "PRO") "ln_pro" "ln_3d")
          "1")
        (start_list "flag")
        (foreach item *opor-dcl-lag-fasteners* (add_list item))
        (end_list)
        (start_list "ftile")
        (foreach item *opor-dcl-tile-fasteners* (add_list item))
        (end_list)
        (set_tile "flag" (itoa (opor-form-mem-get 'lag-fastener-idx 0)))
        (set_tile "ftile" (itoa (opor-form-mem-get 'tile-fastener-idx 0)))
        (set_tile "fstep" (opor-num-str (opor-form-mem-get 'tile-fastener-step 500.0)))
        (set_tile "tri" (if (opor-form-mem-get 'show-triangles nil) "1" "0"))
        (opor-dcl-params-sync)
        (action_tile "cov_d" "(opor-dcl-cover-click \"d\")")
        (action_tile "cov_p" "(opor-dcl-cover-click \"p\")")
        (action_tile "flag" "(opor-dcl-params-sync)")
        (action_tile "ftile" "(opor-dcl-params-sync)")
        (action_tile "accept" "(opor-dcl-params-accept)")
        (action_tile "cancel" "(done_dialog 0)")
        (setq res (start_dialog))
        (unload_dialog id)
        (if (= res 1) *opor-dcl-params-result* nil)))))

;; ---------- диалог «Опора» (ufrm_H) ----------
(defun opor-dcl-choose-support (line / supports id res)
  (setq supports (opor-read-supports line))
  (if (not supports)
    (opor-ui-choose-support line)      ; нет таблицы — ручной ввод как раньше
    (progn
      (setq id (opor-dcl-load))
      (if (not id)
        (opor-ui-choose-support line)
        (if (not (new_dialog "opor_support" id))
          (progn (unload_dialog id) (opor-ui-choose-support line))
          (progn
            (start_list "list")
            (foreach s supports
              (add_list
                (strcat
                  (itoa (cdr (assoc 'index s))) ". "
                  (cdr (assoc 'name s)) "   "
                  (cdr (assoc 'range s)) "   цвет "
                  (itoa (cdr (assoc 'color s))))))
            (end_list)
            (if (>= (opor-form-mem-get 'support-list-idx 0) (length supports))
              (opor-form-mem-set 'support-list-idx 0))
            (set_tile "list" (itoa (opor-form-mem-get 'support-list-idx 0)))
            (action_tile "list"
              "(opor-form-mem-set 'support-list-idx (atoi $value)) (if (= $reason 4) (done_dialog 1))")
            (action_tile "accept"
              "(opor-form-mem-set 'support-list-idx (atoi (get_tile \"list\"))) (done_dialog 1)")
            (action_tile "cancel" "(done_dialog 0)")
            (setq res (start_dialog))
            (unload_dialog id)
            (if (= res 1)
              (nth (opor-form-mem-get 'support-list-idx 0) supports)
              nil)))))))

;; ---------- удобства ----------
;; вид (аналог x_views vb_original: запомнить в начале, вернуть в конце)
(defun opor-view-save ()
  (opor-session-set 'saved-view (list (getvar "VIEWCTR") (getvar "VIEWSIZE"))))

(defun opor-view-show-saved (/ v ctr size half-h half-w scr)
  (setq v (opor-session-get 'saved-view))
  (if v
    (progn
      (setq ctr (car v))
      (setq size (cadr v))
      (setq scr (getvar "SCREENSIZE"))
      (setq half-h (/ size 2.0))
      (setq half-w (* half-h (/ (car scr) (float (cadr scr)))))
      (vl-catch-all-apply 'vla-ZoomWindow
        (list (vlax-get-acad-object)
              (vlax-3d-point (list (- (car ctr) half-w) (- (cadr ctr) half-h) 0.0))
              (vlax-3d-point (list (+ (car ctr) half-w) (+ (cadr ctr) half-h) 0.0)))))))

(defun opor-view-restore ()
  (opor-view-show-saved)
  (opor-session-set 'saved-view nil)
  (princ))

;; слои (замена мёртвого uv_hide_lays; гарантированный возврат через opor-safe-restore)
(defun opor-layers-hide (/ saved name obj target whitelist layers)
  (setq layers (vla-get-Layers (opor-doc)))
  (setq whitelist (mapcar 'strcase *opor-visible-layers*))
  (setq saved '())
  (foreach name *opor-visible-layers*
    (setq obj (vl-catch-all-apply 'vla-Item (list layers name)))
    (if (not (vl-catch-all-error-p obj))
      (progn
        (vl-catch-all-apply 'vla-put-Freeze (list obj :vlax-false))
        (vl-catch-all-apply 'vla-put-LayerOn (list obj :vlax-true)))))
  ;; Сначала уходим на разрешенный слой, иначе старый текущий слой нельзя надежно погасить.
  (setq target (if (opor-layer-exists-p *opor-layer-contour*) *opor-layer-contour* "0"))
  (vl-catch-all-apply 'setvar (list "CLAYER" target))
  (vlax-for obj layers
    (setq name (vla-get-Name obj))
    (if (and (not (member (strcase name) whitelist))
             (equal (vla-get-LayerOn obj) :vlax-true)) ; = на символах не документирован
      (progn
        (setq saved (cons name saved))
        (vl-catch-all-apply 'vla-put-LayerOn (list obj :vlax-false)))))
  (opor-session-set 'hidden-layers saved)
  ;; COM put-LayerOn НЕ обновляет экран (в отличие от команды СЛОЙ) — нужен реген
  (if saved
    (vl-catch-all-apply 'vla-Regen (list (opor-doc) acActiveViewport)))
  (opor-log (strcat "Слоёв скрыто: " (itoa (length saved))))
  (length saved))

(defun opor-layers-restore (/ saved layers obj)
  (setq saved
    (if (and (boundp '*opor-session*) *opor-session*)
      (opor-session-get 'hidden-layers)
      nil))
  (if saved
    (progn
      (setq layers (vla-get-Layers (opor-doc)))
      (foreach name saved
        (setq obj (vl-catch-all-apply 'vla-Item (list layers name)))
        (if (not (vl-catch-all-error-p obj))
          (vl-catch-all-apply 'vla-put-LayerOn (list obj :vlax-true))))
      (opor-session-set 'hidden-layers '())
      ;; включение слоёв тоже требует регена, иначе они не вернутся на экран
      (vl-catch-all-apply 'vla-Regen (list (opor-doc) acActiveViewport))))
  (princ))

;; ---------- сборка session из формы ----------
(defun opor-dcl-fastener-name (idx items line / base)
  (if (<= idx 0)
    nil
    (progn
      (setq base (nth idx items))
      (strcat base (if (= line "3D") " 3D" "")))))

(defun opor-dcl-apply-common (vals / line tile-fastener)
  (setq line (cdr (assoc 'line vals)))
  (opor-session-set 'line line)
  (opor-session-set 'step-x (cdr (assoc 'step-x vals)))
  (opor-session-set 'step-y (cdr (assoc 'step-y vals)))
  (opor-session-set 'tile-size-x (cdr (assoc 'tile-size-x vals)))
  (opor-session-set 'tile-size-y (cdr (assoc 'tile-size-y vals)))
  (opor-session-set 'radius (cdr (assoc 'radius vals)))
  (opor-session-set 'board-length (cdr (assoc 'board-length vals)))
  (opor-session-set 'double-lag-layout (cdr (assoc 'double-lag-layout vals)))
  (opor-session-set 'double-lag-step (cdr (assoc 'double-lag-step vals)))
  (opor-session-set 'lag-axis (cdr (assoc 'lag-axis vals)))
  (opor-session-set 'tile-mode (cdr (assoc 'tile-mode vals))) ; S4: и в Const тоже
  (opor-session-set 'lag-fastener
    (opor-dcl-fastener-name (cdr (assoc 'lag-fastener-idx vals)) *opor-dcl-lag-fasteners* line))
  (if (> (cdr (assoc 'tile-fastener-idx vals)) 0)
    (progn
      (setq tile-fastener
        (opor-dcl-fastener-name
          (cdr (assoc 'tile-fastener-idx vals))
          *opor-dcl-tile-fasteners* line))
      (opor-session-set 'tile-fastener tile-fastener)
      (opor-session-set 'tile-fastener-step
        (if (opor-direct-tile-fastener-p tile-fastener)
          nil
          (cdr (assoc 'tile-fastener-step vals)))))
    (progn
      (opor-session-set 'tile-fastener nil)
      (opor-session-set 'tile-fastener-step nil)))
  (opor-session-set 'show-triangles (cdr (assoc 'show-triangles vals)))
  T)

(defun opor-dcl-read-params (/ vals support pts)
  (setq vals (opor-dcl-show-params "const-height"))
  (cond
    ((not vals)
      (opor-log "Ввод параметров отменён.")
      nil)
    ((not (opor-ui-line-ready-p (cdr (assoc 'line vals))))
      nil)
    (t
      (setq support (opor-dcl-choose-support (cdr (assoc 'line vals))))
      (if (not support)
        (progn (opor-log "Ввод параметров отменён.") nil)
        (progn
          (opor-dcl-apply-common vals)
          (opor-session-set 'support-index (cdr (assoc 'index support)))
          (opor-session-set 'support-name (cdr (assoc 'name support)))
          (opor-session-set 'support-range (cdr (assoc 'range support)))
          (opor-session-set 'support-color (fix (cdr (assoc 'color support))))
          (setq pts (opor-ui-pick-points))
          (if pts
            (progn
              (opor-session-set 'base-point (car pts))
              (opor-session-set 'direction-point (cadr pts))
              (opor-session-set 'table-point (caddr pts))
              T)
            (progn
              (opor-layers-restore)
              (opor-log "Ввод параметров отменён.")
              nil)))))))

(defun opor-dcl-read-var-params (/ vals supports maxmark state pts)
  (setq state nil)
  (while (not state)
    (setq vals (opor-dcl-show-params "var-height"))
    (cond
      ((not vals)
        (opor-log "Ввод параметров переменной высоты отменён.")
        (setq state "cancel"))
      ((not (opor-ui-line-ready-p (cdr (assoc 'line vals))))
        (setq state "fail"))
      (t
        (setq maxmark (opor-level-max-mark *opor-session*))
        (if (and maxmark (>= maxmark (opor-round-half-even (cdr (assoc 'floor-height vals)))))
          (opor-alert
            (strcat
              "Максимальная отметка=" (opor-height-text maxmark)
              "\nУровень чистого пола=" (opor-height-text (cdr (assoc 'floor-height vals)))
              "\nУровень чистого пола должен быть выше максимальной отметки."))
          (setq state "good")))))
  (if (/= state "good")
    nil
    (progn
      (setq supports (opor-read-supports (cdr (assoc 'line vals))))
      (if (not supports)
        (progn
          (opor-log "Таблица 'Опоры' не найдена или пуста; переменная высота не сможет разложить диапазоны.")
          nil)
        (progn
          (opor-dcl-apply-common vals)
          (opor-session-set 'mode "var-height")
          (opor-session-set 'support-ranges supports)
          (opor-session-set 'floor-height (cdr (assoc 'floor-height vals)))
          (opor-session-set 'board-thickness (cdr (assoc 'board-thickness vals)))
          (opor-session-set 'lag-thickness (cdr (assoc 'lag-thickness vals)))
          (opor-session-set 'tile-thickness (cdr (assoc 'tile-thickness vals)))
          (setq pts (opor-ui-pick-points))
          (if pts
            (progn
              (opor-session-set 'base-point (car pts))
              (opor-session-set 'direction-point (cadr pts))
              (opor-session-set 'table-point (caddr pts))
              T)
            (progn
              (opor-layers-restore)
              (opor-log "Ввод параметров переменной высоты отменён.")
              nil)))))))

(princ)
