;;; Разовая загрузка сборки OPOR 3.32-geo-levels для live-теста.
;;;
;;; Зачем отдельный файл. На этой машине рядом лежит вторая распакованная копия
;;; плагина (рабочие_прогоны\_package_diagnostics_2026-07-31\
;;; unpacked_OPOR_NO_GEO_2026-07-31), и автопоиск в 00_ЗАГРУЗИТЬ_OPOR.lsp
;;; цепляет именно её. Здесь папка задана жёстко, поэтому промахнуться нельзя.
;;;
;;; APPLOAD -> выбрать ЭТОТ файл -> «Загрузить».

(vl-load-com)

(setq *opor-root*
  "C:\\Users\\user\\Documents\\Таран\\0_0_ по работке\\плагин дмитрию\\релизы\\OPOR_GEO_2026-07-31\\OPOR")

(if (findfile (strcat *opor-root* "\\opor-loader.lsp"))
  (progn
    (load (strcat *opor-root* "\\opor-loader.lsp"))
    (princ "\n=====================================")
    (princ (strcat "\n  ВЕРСИЯ:        " (if (boundp '*opor-version*) *opor-version* "не определена")))
    (princ (strcat "\n  КОРЕНЬ:        " *opor-root*))
    (princ (strcat "\n  OPORGEOLEVELS: " (if (boundp 'C:OPORGEOLEVELS) "есть" "НЕТ")))
    (princ "\n  Ждём ВЕРСИЯ = 3.32-geo-levels и OPORGEOLEVELS = есть.")
    (princ "\n====================================="))
  (progn
    (princ "\nOPOR: не найден opor-loader.lsp по пути:")
    (princ (strcat "\n  " *opor-root*))
    (princ "\nПроверь, что папка релизы\\OPOR_GEO_2026-07-31 на месте.")))

(princ)
