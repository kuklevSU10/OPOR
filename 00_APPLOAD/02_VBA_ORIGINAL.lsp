(vl-load-com)

(defun opor-appload-project-root (/ source launcher-dir loader selected)
  (setq source
    (cond
      ((and (boundp '*load-truename*) *load-truename*) *load-truename*)
      ((and (boundp '*load-pathname*) *load-pathname*) *load-pathname*)
      (T nil)))
  (cond
    ((and (boundp '*opor-appload-root*)
          *opor-appload-root*
          (findfile (strcat *opor-appload-root* "\\OPOR\\opor-loader.lsp")))
      *opor-appload-root*)
    (source
      (setq launcher-dir (vl-filename-directory source))
      (vl-filename-directory launcher-dir))
    ((setq loader (findfile "OPOR\\opor-loader.lsp"))
      (vl-filename-directory (vl-filename-directory loader)))
    ((setq selected
       (getfiled "Укажите файл OPOR\\opor-loader.lsp" "" "lsp" 0))
      (vl-filename-directory (vl-filename-directory selected)))
    (T nil)))

(defun opor-appload-load (relative / path result)
  (setq path (strcat *opor-appload-root* "\\" relative))
  (setq result (vl-catch-all-apply 'load (list path)))
  (if (vl-catch-all-error-p result)
    (progn
      (alert (strcat "Не удалось загрузить:\n" path "\n\n"
                     (vl-catch-all-error-message result)))
      nil)
    T))

(setq *opor-appload-root* (opor-appload-project-root))
(if *opor-appload-root*
  (setq *opor-appload-root* (vl-string-right-trim "\\/" *opor-appload-root*)))

(if (and *opor-appload-root*
         (opor-appload-load "исходник\\opor2.6\\opor2.6.lsp"))
  (progn
    (opor-appload-load "tools\\diagnostics\\check_dump.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_height_check.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_write_level.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_auto_level.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_slope.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_tables.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_marks.lsp")
    (opor-appload-load "tools\\diagnostics\\probe_lines.lsp")
    (princ "\nОригинальный VBA OPOR загружен. Основная команда: XX.")))

(princ)
