;;; probe_tables.lsp — разведка визуал-dwg (ТЗ п.3): блоки table*, их
;;; атрибуты и вставки. Команда: TBLPROBE. Кодировка: CP1251.
(vl-load-com)

(defun c:TBLPROBE ( / doc blks blk nm cnt ent tags ss i o lay n)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n===== TBLPROBE =====")
  (princ "\n--- Определения блоков table* ---")
  (setq blks (vla-get-Blocks doc) cnt 0)
  (vlax-for blk blks
    (setq nm (vla-get-Name blk))
    (if (and (not (wcmatch nm "`**")) (wcmatch (strcase nm) "TABLE*"))
      (progn
        (setq cnt (1+ cnt) tags "")
        (vlax-for ent blk
          (if (= (vla-get-ObjectName ent) "AcDbAttributeDefinition")
            (setq tags (strcat tags (vla-get-TagString ent) " "))))
        (princ (strcat "\n  " nm " (объектов внутри: "
                       (itoa (vla-get-Count blk)) ")"
                       (if (= tags "")
                         " — атрибутов НЕТ"
                         (strcat "\n      теги: " tags)))))))
  (princ (strcat "\n  Всего table*-определений: " (itoa cnt)))
  (princ "\n--- Вставки (INSERT) table* ---")
  (setq ss (ssget "_X" '((0 . "INSERT"))) n 0)
  (if ss
    (repeat (setq i (sslength ss))
      (setq i (1- i)
            o (vlax-ename->vla-object (ssname ss i))
            nm (vl-catch-all-apply 'vla-get-EffectiveName (list o)))
      (if (vl-catch-all-error-p nm) (setq nm (vla-get-Name o)))
      (if (wcmatch (strcase nm) "TABLE*")
        (progn
          (setq n (1+ n)
                lay (vl-catch-all-apply 'vla-get-Layer (list o)))
          (princ (strcat "\n  " nm " [слой: "
                         (if (vl-catch-all-error-p lay) "?" lay) "]"))))))
  (princ (strcat "\n  Всего table*-вставок: " (itoa n)))
  (setq ss (ssget "_X" '((0 . "ACAD_TABLE"))))
  (princ (strcat "\n--- ACAD_TABLE в чертеже: " (if ss (itoa (sslength ss)) "0")))
  (princ "\n===== КОНЕЦ TBLPROBE =====")
  (princ))

(princ "\nprobe_tables загружен. Команда: TBLPROBE")
(princ)
