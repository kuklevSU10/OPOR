;;; probe_marks.lsp v2 — диагностика: где блоки отметок и что лежит на слоях.
;;; Команда: MARKDUMP. Кодировка: CP1251.
(vl-load-com)

;; тип объекта с защитой: entget бывает капризный
(defun pm-etype (e / d tp)
  (setq d (vl-catch-all-apply 'entget (list e)))
  (cond
    ((vl-catch-all-error-p d) "<entget-ошибка>")
    ((null d) "<entget=nil>")
    ((null (setq tp (cdr (assoc 0 d)))) "<без-кода-0>")
    (t tp)))

;; COM-имя объекта с защитой
(defun pm-objname (e / o nm)
  (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (vl-catch-all-error-p o)
    "<vla-ошибка>"
    (progn
      (setq nm (vl-catch-all-apply 'vla-get-ObjectName (list o)))
      (if (vl-catch-all-error-p nm) "<без-ObjectName>" nm))))

(defun pm-layer-report (ln / doc lay ss i e tp tbl pr bad)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq lay (vl-catch-all-apply '(lambda () (vla-Item (vla-get-Layers doc) ln))))
  (if (vl-catch-all-error-p lay)
    (princ (strcat "\n  " ln ": НЕТ ТАКОГО СЛОЯ"))
    (progn
      (setq ss (ssget "_X" (list (cons 8 ln))) tbl nil bad nil)
      (princ (strcat "\n  " ln ": on="
                     (if (equal (vla-get-LayerOn lay) :vlax-true) "1" "0")
                     " frozen="
                     (if (equal (vla-get-Freeze lay) :vlax-true) "1" "0")
                     " ssget=" (if ss (itoa (sslength ss)) "0")))
      (if ss
        (repeat (setq i (sslength ss))
          (setq i (1- i)
                e (ssname ss i)
                tp (pm-etype e))
          (if (wcmatch tp "<*")
            (if (< (length bad) 3) (setq bad (cons e bad))))
          (if (setq pr (assoc tp tbl))
            (setq tbl (subst (cons tp (1+ (cdr pr))) pr tbl))
            (setq tbl (cons (cons tp 1) tbl)))))
      (foreach pr tbl
        (princ (strcat "\n      " (car pr) " x" (itoa (cdr pr)))))
      (foreach e bad
        (princ (strcat "\n      аномалия: " (vl-princ-to-string e)
                       " COM=" (pm-objname e))))))
  (princ))

(defun c:MARKDUMP ( / ss i o nm pt att n total ly)
  (princ "\n===== MARKDUMP v2 =====")
  (princ "\n--- Блоки otmetka* по всему чертежу ---")
  (setq ss (ssget "_X" '((0 . "INSERT"))) n 0 total 0)
  (if ss
    (repeat (setq i (sslength ss))
      (setq i (1- i)
            o (vl-catch-all-apply 'vlax-ename->vla-object (list (ssname ss i))))
      (if (not (vl-catch-all-error-p o))
        (progn
          (setq total (1+ total)
                nm (vl-catch-all-apply 'vla-get-EffectiveName (list o)))
          (if (vl-catch-all-error-p nm)
            (setq nm (vl-catch-all-apply 'vla-get-Name (list o))))
          (if (and (not (vl-catch-all-error-p nm)) nm
                   (wcmatch (strcase nm) "*OTMETKA*"))
            (progn
              (setq n (1+ n)
                    ly (vl-catch-all-apply 'vla-get-Layer (list o))
                    pt (vl-catch-all-apply '(lambda () (vlax-get o 'InsertionPoint)))
                    att (vl-catch-all-apply
                          '(lambda ( / a)
                             (setq a (vlax-invoke o 'GetAttributes))
                             (if a (vla-get-TextString (car a)) "нет атрибутов"))))
              (princ (strcat "\n  " nm
                             " [слой: "
                             (if (vl-catch-all-error-p ly) "?" ly)
                             "]"
                             (if (vl-catch-all-error-p pt)
                               ""
                               (strcat " @" (rtos (car pt) 2 0) "," (rtos (cadr pt) 2 0)))
                             " знач="
                             (if (vl-catch-all-error-p att)
                               "?"
                               (vl-princ-to-string att))))))))))
  (princ (strcat "\n  Итого otmetka-блоков: " (itoa n)
                 " (всего INSERT в чертеже: " (itoa total) ")"))
  (princ "\n--- Слои и их содержимое ---")
  (foreach ln '("контур" "опорыvb" "опоры_текстvb" "сеткаvb" "плиткаvb"
                "областиvb" "линии_высот" "линии_высот3")
    (pm-layer-report ln))
  (princ "\n===== КОНЕЦ MARKDUMP =====")
  (princ))

(princ "\nprobe_marks v2 загружен. Команда: MARKDUMP")
(princ)
