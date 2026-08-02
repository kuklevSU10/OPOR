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

(if *opor-appload-root*
  (foreach relative
  '("tools\\generators\\make_etalons.lsp"
    "tools\\generators\\make_slope_etalons.lsp"
    "tools\\generators\\make_height_check_etalons.lsp"
    "tools\\generators\\make_write_level_etalons.lsp"
    "tools\\generators\\make_auto_level_etalons.lsp"
    "tools\\generators\\make_ring_etalons.lsp"
    "tools\\generators\\make_regcheck_etalons.lsp"
    "tools\\generators\\make_multicontour_etalons.lsp"
    "tools\\generators\\make_multicontour_var_etalons.lsp"
    "tools\\generators\\make_tin_etalons.lsp"
    "tools\\generators\\make_tin_robust_etalons.lsp"
    "tools\\diagnostics\\check_dump.lsp"
    "tools\\diagnostics\\probe_height_check.lsp"
    "tools\\diagnostics\\probe_write_level.lsp"
    "tools\\diagnostics\\probe_auto_level.lsp"
    "tools\\diagnostics\\probe_ring.lsp"
    "tools\\diagnostics\\probe_slope.lsp"
    "tools\\diagnostics\\probe_tables.lsp"
    "tools\\diagnostics\\probe_total_tables.lsp"
    "tools\\diagnostics\\probe_marks.lsp"
    "tools\\diagnostics\\probe_lines.lsp"
      "tools\\diagnostics\\probe_tin.lsp")
    (opor-appload-load relative)))

(princ "\nТестовые инструменты загружены: ETS1/ETS2/ETS3/ETS7/ETS8/ETS9/ETS10/ETS11/ETS12/ETS13/ETS14/ETS15/ETS16 и диагностика.")
(princ)
