;;; OPOR clean AutoLISP port - configuration

(setq *opor-version* "3.35-customer-polish") ; таблицы, draw order опор, PRO-only Slope, целые уклоны
(setq *opor-xdata-app* "OPOR")

(setq *opor-layer-contour* "контур")
(setq *opor-layer-grid* "сеткаvb")
(setq *opor-layer-supports* "опорыvb")
(setq *opor-layer-support-text* "опоры_текстvb")
(setq *opor-layer-tiles* "плиткаvb")
(setq *opor-layer-holes* "областиvb")
(setq *opor-layer-level-lines* "линии_высот")
(setq *opor-layer-temp* "templayvb")
(setq *opor-layer-tables* "0")
(setq *opor-level-block-name* "otmetka_oporvb")
(setq *opor-geo-source-layer* "GEO_POINTS")
(setq *opor-geo-meter-drawing-threshold* 1000.0)
(setq *opor-geo-default-step-m* 1.0)
(setq *opor-geo-default-step-mm* 1000.0)
(setq *opor-geo-max-created-marks* 2000)

(setq *opor-layer-defs*
  (list
    (list *opor-layer-level-lines* 8 "Continuous")
    (list *opor-layer-grid* 8 "Continuous")
    (list *opor-layer-supports* 7 "Continuous")
    (list *opor-layer-support-text* 9 "Continuous")
    (list *opor-layer-tiles* 9 "Continuous")
    (list *opor-layer-holes* 1 "Continuous")
    (list *opor-layer-temp* 8 "Continuous")))

(setq *opor-block-by-line*
  (list
    (cons "lev" "opor_symb")
    (cons "3D" "opor_symb3d")
    (cons "PRO" "opor_symbPRO")))

(setq *opor-total-table-by-line*
  (list
    (cons "lev" "table_totl_1")
    (cons "3D" "table_totl_3D")
    (cons "PRO" "table_totl_PRO")))

(setq *opor-table-block-library* "opor-table-blocks.dwg")
(setq *opor-new-3d-support-table-block* "OPOR_SUPPORT_SPEC_3D")
(setq *opor-new-pro-support-table-block* "OPOR_SUPPORT_SPEC_PRO")
(setq *opor-new-tile-params-block* "OPOR_PARAMS_TILE")
(setq *opor-new-board-params-block* "OPOR_PARAMS_BOARD")
(setq *opor-new-extra-row-block* "OPOR_PARAMS_EXTRA_ROW")
(setq *opor-new-extra-row-height* 800.0)
(setq *opor-fastener-thickness* 4.0)
(setq *opor-new-board-params-height* 5600.0)
(setq *opor-new-tile-params-height* 3200.0)
(setq *opor-drawing-title-prefix* "Разбивка опор Level")
(setq *opor-drawing-title-style* "isocpeur")
(setq *opor-drawing-title-font* "isocpeur.ttf")
;; ТЗ: размер 5. В штатном DWG оформление 1:100: TEXTSIZE=500.
(setq *opor-drawing-title-height* 500.0)
(setq *opor-drawing-title-offset* 1000.0)
(setq *opor-dimstyle-name* "ISO-25")
(setq *opor-dimstyle-text-style* "isocpeur")
(setq *opor-dimstyle-arrow-size* 2.5)
(setq *opor-dimstyle-center-size* 2.5)
(setq *opor-dimstyle-line-spacing* 3.75)
(setq *opor-dimstyle-text-height* 2.5)
(setq *opor-dimstyle-text-gap* 0.625)
(setq *opor-dimstyle-jog-height-factor* 1.5)
(setq *opor-dimstyle-text-alignment* 0) ; DIMTALN: according to ISO
;; В актуальной библиотеке база спецификации находится на её нижней границе:
;; спецификация растёт вверх, а динамические строки параметров — вниз от той же точки.
(setq *opor-new-3d-params-offset-y* 0.0)
(setq *opor-new-pro-params-offset-y* 0.0)
(setq *opor-new-3d-support-tags*
  (list
    "SUP_3D_35_50"
    "SUP_3D_50_80"
    "SUP_3D_80_140"
    "SUP_3D_95_155"
    "SUP_3D_145_240"
    "SUP_3D_160_270"
    "SUP_3D_205_340"
    "SUP_3D_235_400"
    "SUP_3D_315_530"
    "SUP_3D_390_660"))
(setq *opor-new-pro-support-tags*
  (list
    "SUP_PRO_LOW12"
    "SUP_PRO_LOW20"
    "SUP_PRO_25_35"
    "SUP_PRO_35_50"
    "SUP_PRO_50_80"
    "SUP_PRO_80_140"
    "SUP_PRO_95_155"
    "SUP_PRO_145_240"
    "SUP_PRO_160_270"
    "SUP_PRO_205_340"
    "SUP_PRO_235_400"
    "SUP_PRO_315_530"
    "SUP_PRO_390_660"))

(setq *opor-known-blocks*
  (list
    "otmetka_oporvb"
    "opor_symb"
    "opor_symb3d"
    "opor_symbPRO"
    "table_totl_1"
    "table_totl_3D"
    "table_totl_PRO"
    "OPOR_SUPPORT_SPEC_3D"
    "OPOR_SUPPORT_SPEC_PRO"
    "OPOR_PARAMS_TILE"
    "OPOR_PARAMS_BOARD"
    "table_plitka"
    "table_lag"
    "table_slope"
    "slope"
    "проверкаvb3"))

(setq *opor-mvp-required-blocks*
  (list "opor_symb3d" "opor_symbPRO"))

(setq *opor-default-params*
  (list
    (cons 'mode "const-height")
    (cons 'line "3D")
    (cons 'tile-mode "d")
    (cons 'lag-axis "vect")
    (cons 'step-x 600.0)
    (cons 'step-y 600.0)
    (cons 'radius 100.0)
    (cons 'floor-height 101.0)
    (cons 'board-thickness 27.0)
    (cons 'lag-thickness 49.0)
    (cons 'tile-thickness 20.0)
    (cons 'tile-size-x 600.0)
    (cons 'tile-size-y 600.0)
    (cons 'board-width 160.0)
    (cons 'board-length 3000.0)
    (cons 'lag-width 40.0)
    (cons 'double-lag-layout "none")
    (cons 'double-lag-step 0.0)
    (cons 'support-name "OPOR")
    (cons 'support-range "-")
    (cons 'support-color 256)
    (cons 'support-index 1)))

(setq *opor-point-tolerance* 0.001)
(setq *opor-curve-chord-tolerance* 2.0)         ; макс. стрела хорды при разбиении дуг для TIN/Var, мм
(setq *opor-curve-max-pieces-per-segment* 720) ; защита от чрезмерной детализации одной дуги
(setq *opor-tin-retry-constraint-length* 4000.0) ; шаг техточек retry для невосстановленного длинного ребра, мм
(setq *opor-vba-vertex-border-tolerance* 20.0)   ; b2_mains: dst < 20
(setq *opor-vba-point-dedupe-tolerance* 0.01)    ; equalpointsDel/equalpointsDel2: < 0.01
(setq *opor-vba-node-self-dedupe-tolerance* 0.01)
(setq *opor-vba-node-border-tolerance* 0.01)     ; equalpointsDel2(arrnods, arrborder): < 0.01
(setq *opor-vba-min-grid-line-length* 10.0)      ; trimXL: Length > 10
(setq *opor-vba-mark-match-tolerance* 1.0)       ; chk_blk_contr/getH_vertecs: |dx|<1 и |dy|<1
(setq *opor-vba-border-line-tolerance* 10.0)     ; getH_bords: lindist < 10
(setq *opor-support-dedupe-tolerance* 1.0)       ; схлопнуть повторные опоры в одной физической точке
(setq *opor-support-max-overlap* 80.0)           ; если круги опор перекрываются больше, одну опору удаляем
(setq *opor-vba-dbl-lag-dedupe-tolerance* 10.0)  ; trimXL: maxDst = 10 (дедуп доп. лаг)
(setq *opor-dbl-lag-color* 12)                   ; copyxlinlag: newxl.color = 12
(setq *opor-vba-tile-outside-area-tolerance* 0.001) ; trimplitk: cregarea < 0.001 - плитка снаружи
(setq *opor-vba-tile-trim-area-tolerance* 1.0)      ; trimplitk: plarea-cregarea <= 1 - касание
(setq *opor-vba-tile-hole-zero-area-tolerance* 0.0001) ; trimPlitkOpp: areaNew < 0.0001
(setq *opor-error-color* 30)                     ; оранжевый: маркеры ошибок высот
(setq *opor-error-circle-radius* 500.0)          ; AddCircle(cp, 100*5)
(setq *opor-slope-mark-tolerance* 100.0)         ; find_blk: отметка в радиусе <= 100 мм
(setq *opor-slope-boundary-tolerance* 1.0)       ; isPntInPolygon modd=1: окружность R=1
(setq *opor-slope-min-percent* 2.0)
(setq *opor-slope-max-percent* 9.0)
(setq *opor-boundary-lag-length-mode* "all")     ; VBA: контур И проёмы ("none"/"holes"/"all")
(setq *opor-keep-perp-grid* nil)                 ; T = рисовать поперечное семейство как VBA (A/B-сверка)
(setq *opor-use-dcl* T)                          ; nil = командный ввод (нужен для перегона эталонов: DCL не пишется в лог)
(setq *opor-layer-triangles* "линии_высот3")     ; «показать разбивку» (h_triang)
(setq *opor-visible-layers*                      ; видны при выборе контура после A/B
  (list *opor-layer-contour* *opor-layer-holes* "0"))
(setq *opor-grid-margin-factor* 2.5)

;; Состав пола из таблиц заказчика (подтверждено 23.07.2026):
;; TOP/CLIP держат лагу; TILE крепит плитку к лаге; LASTRA ставит плитку
;; напрямую на опору. Каждый выбранный вид крепежа добавляет 4 мм.
(defun opor-fastener-name-has-p (name fragment)
  (and name
       fragment
       (not (null (vl-string-search (strcase fragment) (strcase name))))))

(defun opor-direct-tile-fastener-p (name)
  (opor-fastener-name-has-p name "LASTRA"))

(defun opor-lag-tile-fastener-p (name)
  (and (opor-fastener-name-has-p name "TILE")
       (not (opor-direct-tile-fastener-p name))))

(defun opor-floor-composition-values (tile-mode lag-fastener tile-fastener)
  (cond
    ((= tile-mode "d") "board-lag")
    ((opor-direct-tile-fastener-p tile-fastener) "tile-direct")
    ((or lag-fastener (opor-lag-tile-fastener-p tile-fastener)) "tile-lag")
    ;; Старые сохранённые настройки без крепежа оставляем в прежнем режиме.
    (t "tile-legacy")))

(defun opor-floor-composition ()
  (opor-floor-composition-values
    (opor-session-get 'tile-mode)
    (opor-session-get 'lag-fastener)
    (opor-session-get 'tile-fastener)))

(defun opor-floor-uses-lags-p ()
  (/= (opor-floor-composition) "tile-direct"))

(defun opor-floor-height-uses-lag-p ()
  (member (opor-floor-composition) '("board-lag" "tile-lag")))

(defun opor-floor-uses-lag-param-table-p ()
  (member (opor-floor-composition) '("board-lag" "tile-lag")))

(defun opor-floor-fastener-count ()
  (+ (if (opor-session-get 'lag-fastener) 1 0)
     (if (opor-session-get 'tile-fastener) 1 0)))

(defun opor-floor-fastener-thickness ()
  (* *opor-fastener-thickness* (opor-floor-fastener-count)))

(defun opor-floor-composition-error-values (tile-mode lag-fastener tile-fastener)
  (cond
    ((and (= tile-mode "d") tile-fastener)
      "Для доски крепление плитки не используется.")
    ((and (= tile-mode "p")
          (opor-direct-tile-fastener-p tile-fastener)
          lag-fastener)
      "Level Lastra используется без лаг: выберите для лаг «Нет».")
    ((and (= tile-mode "p")
          (opor-lag-tile-fastener-p tile-fastener)
          (not lag-fastener))
      "Для Level Tile выберите крепление лаг Level TOP или Level Clip.")
    (t nil)))

(princ)
