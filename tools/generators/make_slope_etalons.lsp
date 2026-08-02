;;; make_slope_etalons.lsp — ETS7, эталон slopeWR + slope.
;;; Запускать в НОВОМ чертеже из шаблона с блоками slope/table_slope,
;;; otmetka_oporvb и opor_symb. Команда: ETS7.

(vl-load-com)

(defun ets7-doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun ets7-ms ()
  (vla-get-ModelSpace (ets7-doc)))

(defun ets7-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 2 name)
        (cons 70 0)
        (cons 62 color)
        (cons 6 "Continuous")))))

(defun ets7-rect (x1 y1 x2 y2 layer)
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDbPolyline")
      (cons 90 4)
      (cons 70 1)
      (list 10 x1 y1)
      (list 10 x2 y1)
      (list 10 x2 y2)
      (list 10 x1 y2))))

(defun ets7-set-first-attribute (block text / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq raw (vlax-variant-value raw))
      (if (= (type raw) 'safearray)
        (progn
          (setq atts (vlax-safearray->list raw))
          (if atts (vla-put-TextString (car atts) text)))))))

(defun ets7-insert (name x y layer color text / value block)
  (setq value
    (vl-catch-all-apply
      'vla-InsertBlock
      (list
        (ets7-ms)
        (vlax-3d-point (list x y 0.0))
        name 1.0 1.0 1.0 0.0)))
  (if (vl-catch-all-error-p value)
    (progn
      (princ
        (strcat "\n[ETS7] ОШИБКА вставки " name ": "
          (vl-catch-all-error-message value)))
      nil)
    (progn
      (setq block value)
      (if layer (vl-catch-all-apply 'vla-put-Layer (list block layer)))
      (if color (vl-catch-all-apply 'vla-put-Color (list block color)))
      (if text (ets7-set-first-attribute block text))
      block)))

(defun ets7-blocks-ready-p (/ missing)
  (setq missing '())
  (foreach name '("slope" "table_slope" "otmetka_oporvb" "opor_symb")
    (if (not (tblsearch "BLOCK" name))
      (setq missing (cons name missing))))
  (if missing
    (progn
      (princ "\n[ETS7] ОШИБКА: в шаблоне не найдены блоки:")
      (foreach name (reverse missing) (princ (strcat " " name)))
      nil)
    T))

(defun ets7-make-region (x y percent / dh)
  (setq dh (* percent 10.0))
  (ets7-rect x y (+ x 1000.0) (+ y 1000.0) "линии_высот")
  ;; Порядок вершин даёт pmin=(x,y), pmax=(x+1000,y), dist=1000.
  (ets7-insert "otmetka_oporvb" x y "линии_высот" nil "0")
  (ets7-insert "otmetka_oporvb" (+ x 1000.0) y "линии_высот" nil (rtos dh 2 0))
  (ets7-insert "otmetka_oporvb" (+ x 1000.0) (+ y 1000.0) "линии_высот" nil (rtos dh 2 0))
  (ets7-insert "otmetka_oporvb" x (+ y 1000.0) "линии_высот" nil "0")
  (ets7-insert "slope" (+ x 500.0) (+ y 500.0) "0" 256 "?")
  ;; Две опоры внутри и одна на границе: проверяется modd=1 из VBA.
  (ets7-insert "opor_symb" (+ x 250.0) (+ y 350.0) "опорыvb" 7 "-")
  (ets7-insert "opor_symb" (+ x 750.0) (+ y 650.0) "опорыvb" 7 "-")
  (ets7-insert "opor_symb" (+ x 500.0) y "опорыvb" 7 "-")
  T)

(defun c:ETS7 (/ x p)
  (if (not (ets7-blocks-ready-p))
    nil
    (progn
      (ets7-ensure-layer "контур" 7)
      (ets7-ensure-layer "линии_высот" 8)
      (ets7-ensure-layer "опорыvb" 7)
      (ets7-ensure-layer "опоры_текстvb" 9)
      ;; Два внешних контура по пять областей — проверка мультиконтурного Slope.
      (ets7-rect -200.0 -200.0 6400.0 1200.0 "контур")
      (ets7-rect -200.0 2300.0 6400.0 3700.0 "контур")
      (setq p 1 x 0.0)
      (while (<= p 5)
        (ets7-make-region x 0.0 p)
        (setq p (1+ p) x (+ x 1300.0)))
      (setq p 6 x 0.0)
      (while (<= p 10)
        (ets7-make-region x 2500.0 p)
        (setq p (1+ p) x (+ x 1300.0)))
      (princ "\n[ETS7] Готово: два контура, области 1%...10%, по 3 опоры.")
      (princ "\n[ETS7] Прогон: OPORSLOPEWR на каждом контуре; затем OPORSLOPE:")
      (princ "\n[ETS7] выбрать оба контура по очереди, затем Enter.")
      (princ "\n[ETS7] Ожидание Slope: удалено=3; P2...P9=3; P2-ALL=21; P3-ALL=30.")
      T))
  (princ))

(princ "\n[ETS7] make_slope_etalons загружен. Команда: ETS7.")
(princ)
