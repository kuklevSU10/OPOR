;;; ============================================================================
;;;  check_dump.lsp  —  инструмент замера состояния для golden-master сверки
;;;  Снимает объективный «снимок» результата работы плагина, чтобы сравнить
;;;  ОРИГИНАЛ и ПЕРЕПИСАННУЮ версию на идентичность.
;;;
;;;  Команды:
;;;    DUMPALL    — всё разом (главная: запускать после прогона плагина)
;;;    DUMPOPOR   — опоры: всего + разбивка по цветам (=диапазонам высот)
;;;    DUMPLAG    — лаги: число линий и суммарная длина (мм и м)
;;;    DUMPAREA   — площадь: контур, областиvb, и разность (контур-областиvb)
;;;    DUMPLAYERS — слои плагина: вкл/выкл + число объектов
;;;    DUMPTABLE  — все таблицы ACAD_TABLE с содержимым ячеек
;;;
;;;  Загрузка: APPLOAD -> check_dump.lsp
;;;  ВАЖНО: имена слоёв ниже — под шаблон LEVEL. Если контур у тебя на другом
;;;  слое — поправь переменную *contour-layer* ниже.
;;; ============================================================================

(vl-load-com)

(setq *contour-layer* "контур")   ; <-- слой внешнего контура (поправь при необходимости)
(setq *plugin-layers*              ; слои, которые показывает DUMPLAYERS
  (list "контур" "опорыvb" "опоры_текстvb" "сеткаvb"
        "плиткаvb" "областиvb" "линии_высот"))

(defun *ad* () (vla-get-ActiveDocument (vlax-get-acad-object)))

;; -------- опоры: всего + по цветам ----------------------------------------
(defun c:dumpopor ( / ss i o col tot tbl found)
  (setq ss (ssget "_X" '((0 . "INSERT") (8 . "опорыvb")))
        tot 0 tbl '())
  (if ss
    (repeat (setq i (sslength ss))
      (setq i   (1- i)
            o   (vlax-ename->vla-object (ssname ss i))
            col (vla-get-Color o)
            tot (1+ tot))
      (if (setq found (assoc col tbl))
        (setq tbl (subst (cons col (1+ (cdr found))) found tbl))
        (setq tbl (cons (cons col 1) tbl)))))
  (setq tbl (vl-sort tbl '(lambda (a b) (< (car a) (car b)))))
  (princ (strcat "\n--- ОПОРЫ (слой опорыvb) --- ВСЕГО: " (itoa tot)))
  (foreach pr tbl
    (princ (strcat "\n    цвет " (itoa (car pr)) " : " (itoa (cdr pr)) " шт"
                   (if (= (car pr) 30) "   <-- ОШИБКА высоты (оранжевый)" ""))))
  (if (= tot 0) (princ "  (нет блоков — проверь имя слоя/кодировку)"))
  (princ))

;; -------- лаги: число линий + длина ---------------------------------------
(defun c:dumplag ( / ss i o tot q)
  (setq ss (ssget "_X" '((0 . "LINE") (8 . "сеткаvb"))) tot 0.0 q 0)
  (if ss
    (repeat (setq i (sslength ss))
      (setq i (1- i) o (vlax-ename->vla-object (ssname ss i))
            tot (+ tot (vla-get-Length o)) q (1+ q))))
  (princ (strcat "\n--- ЛАГИ (LINE на сеткаvb) --- линий: " (itoa q)
                 "  длина: " (rtos tot 2 1) " мм = " (rtos (/ tot 1000.0) 2 2) " м"))
  (princ))

;; -------- площадь по слою --------------------------------------------------
(defun layer-area (lay / ss i o a)
  (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 lay))) a 0.0)
  (if ss
    (repeat (setq i (sslength ss))
      (setq i (1- i) o (vlax-ename->vla-object (ssname ss i)))
      (if (vlax-property-available-p o 'Area)
        (setq a (+ a (vla-get-Area o))))))
  a)

(defun c:dumparea ( / ac ao)
  (setq ac (layer-area *contour-layer*) ao (layer-area "областиvb"))
  (princ (strcat "\n--- ПЛОЩАДЬ ---"
                 "\n    контур ('" *contour-layer* "'): " (rtos (/ ac 1e6) 2 3) " м2"
                 "\n    областиvb: " (rtos (/ ao 1e6) 2 3) " м2"
                 "\n    контур - областиvb: " (rtos (/ (- ac ao) 1e6) 2 3) " м2"))
  (princ))

;; -------- слои: состояние + число объектов --------------------------------
(defun layer-info (ln / lobj st ss n)
  (setq lobj (vl-catch-all-apply
               '(lambda () (vla-item (vla-get-Layers (*ad*)) ln))))
  (if (vl-catch-all-error-p lobj)
    "нет такого слоя"
    (progn
      (setq st (if (= (vla-get-LayerOn lobj) :vlax-true) "вкл " "ВЫКЛ")
            ss (ssget "_X" (list (cons 8 ln)))
            n  (if ss (sslength ss) 0))
      (strcat st ", объектов: " (itoa n)))))

(defun c:dumplayers ( / )
  (princ "\n--- СЛОИ ПЛАГИНА ---")
  (foreach ln *plugin-layers*
    (princ (strcat "\n    " ln " : " (layer-info ln))))
  (princ))

;; -------- таблицы ----------------------------------------------------------
(defun c:dumptable ( / ss i tb r c rr cc)
  (setq ss (ssget "_X" '((0 . "ACAD_TABLE"))))
  (if (null ss)
    (princ "\n--- ТАБЛИЦЫ --- не найдено")
    (repeat (setq i (sslength ss))
      (setq i (1- i)
            tb (vlax-ename->vla-object (ssname ss i))
            r  (vla-get-Rows tb)
            c  (vla-get-Columns tb))
      (princ (strcat "\n--- ТАБЛИЦА [" (itoa r) " x " (itoa c) "] ---"))
      (setq rr 0)
      (repeat r
        (princ "\n   ")
        (setq cc 0)
        (repeat c
          (princ (strcat "|"
                   (vl-princ-to-string
                     (vl-catch-all-apply 'vla-GetText (list tb rr cc)))))
          (setq cc (1+ cc)))
        (princ "|")
        (setq rr (1+ rr)))))
  (princ))

;; -------- всё разом --------------------------------------------------------
(defun c:dumpall ( / )
  (princ "\n========== СНИМОК СОСТОЯНИЯ ==========")
  (c:dumpopor) (c:dumplag) (c:dumparea) (c:dumplayers) (c:dumptable)
  (princ "\n=====================================")
  (princ))

(princ "\ncheck_dump.lsp загружен. Главная команда: DUMPALL")
(princ)
