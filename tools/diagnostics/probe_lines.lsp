;;; Зонд: печатает каждую LINE на слое "сеткаvb" — точки, длину, цвет.
;;; APPLOAD -> команда LINEDUMP

(vl-load-com)

(defun c:LINEDUMP (/ ss idx en ed sp ep len n)
  (setq ss (ssget "_X" '((0 . "LINE") (8 . "сеткаvb"))))
  (princ "\n===== LINES on сеткаvb =====")
  (if (not ss)
    (princ "\nнет линий")
    (progn
      (setq idx 0)
      (setq n (sslength ss))
      (while (< idx n)
        (setq en (ssname ss idx))
        (setq ed (entget en))
        (setq sp (cdr (assoc 10 ed)))
        (setq ep (cdr (assoc 11 ed)))
        (setq len (distance sp ep))
        (princ
          (strcat
            "\n#" (itoa (1+ idx))
            "  (" (rtos (car sp) 2 1) "," (rtos (cadr sp) 2 1) ")"
            " -> (" (rtos (car ep) 2 1) "," (rtos (cadr ep) 2 1) ")"
            "  L=" (rtos len 2 1)
            "  цвет=" (itoa (if (assoc 62 ed) (cdr (assoc 62 ed)) 256))))
        (setq idx (1+ idx)))
      (princ (strcat "\nвсего: " (itoa n)))))
  (princ "\n============================")
  (princ))

(princ "\nprobe_lines.lsp загружен. Команда: LINEDUMP")
(princ)
