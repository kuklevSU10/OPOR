;;; OPOR clean AutoLISP port loader.
;;; APPLOAD this file. Commands: OPOR, XX, OPORSLOPE, OPORSLOPEWR, OPORHEIGHTCHECK, OPORWRITELEVEL, OPORAUTOLEVEL, OPORCHECK, OPORCLEAN, OPORDUMP, OPORTABLES, OPOROLD, OPORSHOW, OPORDEBUG.

(vl-load-com)

;; Не привязываем выдачу к машине разработчика. Обычно корень определяется по
;; *load-truename*/*load-pathname*; nil оставляет только переносимые кандидаты.
(setq *opor-install-root* nil)

(defun opor-trim-dir (dir)
  (if dir (vl-string-right-trim "\\/" dir) nil))

(defun opor-dir-file (dir name)
  (strcat (opor-trim-dir dir) "\\" name))

(defun opor-dir-has-module-p (dir name)
  (and dir (/= dir "") (findfile (opor-dir-file dir name))))

(defun opor-add-candidate-dir (dir dirs)
  (setq dir (opor-trim-dir dir))
  (if (and dir (/= dir "") (not (member dir dirs)))
    (cons dir dirs)
    dirs))

(defun opor-loader-candidates (/ dirs found dwgprefix)
  (setq dirs '())
  (if (and (boundp '*opor-root*) *opor-root*)
    (setq dirs (opor-add-candidate-dir *opor-root* dirs)))
  (if (and (boundp '*load-pathname*) *load-pathname*)
    (setq dirs (opor-add-candidate-dir (vl-filename-directory *load-pathname*) dirs)))
  (if (and (boundp '*load-truename*) *load-truename*)
    (setq dirs (opor-add-candidate-dir (vl-filename-directory *load-truename*) dirs)))
  (if (setq found (findfile "opor-loader.lsp"))
    (setq dirs (opor-add-candidate-dir (vl-filename-directory found) dirs)))
  (if (setq found (findfile "OPOR\\opor-loader.lsp"))
    (setq dirs (opor-add-candidate-dir (vl-filename-directory found) dirs)))
  (setq dwgprefix (getvar "DWGPREFIX"))
  (if dwgprefix
    (progn
      (setq dirs (opor-add-candidate-dir (strcat dwgprefix "OPOR") dirs))
      (setq dirs (opor-add-candidate-dir dwgprefix dirs))))
  (setq dirs (opor-add-candidate-dir *opor-install-root* dirs))
  (reverse dirs))

(defun opor-loader-root (/ dirs root)
  (setq dirs (opor-loader-candidates))
  (setq root
    (vl-some
      '(lambda (dir)
         (if (opor-dir-has-module-p dir "opor-config.lsp") dir nil))
      dirs))
  (if root root "."))

(setq *opor-root* (opor-loader-root))

(defun opor-loader-path (name)
  (strcat *opor-root* "\\" name))

(defun opor-loader-candidates-text (/ text)
  (setq text "")
  (foreach dir (opor-loader-candidates)
    (setq text (strcat text "\n- " dir)))
  text)

(defun opor-load-module (name / path found result)
  (setq path (opor-loader-path name))
  (setq found (findfile path))
  (if found
    (progn
      (setq result (vl-catch-all-apply 'load (list found)))
      (if (vl-catch-all-error-p result)
        (progn
          (alert
            (strcat
              "OPOR: ошибка загрузки модуля\n"
              name
              "\n\n"
              (vl-catch-all-error-message result)))
          (exit))
        result))
    (progn
      (alert
        (strcat
          "OPOR: не найден модуль\n"
          path
          "\n\nПроверенные папки:"
          (opor-loader-candidates-text)))
      (exit))))

(foreach module
  (list
    "opor-config.lsp"
    "opor-errors.lsp"
    "opor-geometry.lsp"
    "opor-cleanup.lsp"
    "opor-dump.lsp"
    "opor-tables.lsp"
    "opor-ui.lsp"
    "opor-dcl.lsp"
    "opor-trim.lsp"
    "opor-grid.lsp"
    "opor-supports.lsp"
    "opor-levels.lsp"
    "opor-tiles.lsp"
    "opor-lags.lsp"
    "opor-slope.lsp"
    "opor-height-check.lsp"
    "opor-write-level.lsp"
    "opor-auto-level.lsp"
    "opor-ring.lsp"
    "opor-tin.lsp"
    "opor-core.lsp")
  (opor-load-module module))

(defun c:OPOR ()
  (opor-run-safe 'opor-main))

(defun c:XX ()
  (c:OPOR))

(defun c:OPORSLOPE ()
  (opor-run-safe 'opor-command-slope))

(defun c:OPORSLOPEWR ()
  (opor-run-safe 'opor-command-slope-write))

(defun c:OPORHEIGHTCHECK ()
  (opor-run-safe 'opor-command-height-check))

(defun c:OPORWRITELEVEL ()
  (opor-run-safe 'opor-command-write-level))

(defun c:OPORAUTOLEVEL ()
  (opor-run-safe 'opor-command-auto-level))

(defun c:OPORRING ()
  (opor-run-safe 'opor-command-ring))

(defun c:OPORTIN ()
  (opor-run-safe 'opor-command-tin))

(defun c:OPORREGCHECK ()
  (opor-run-safe 'opor-command-region-check))

(defun c:OPORCHECK ()
  (opor-run-safe 'opor-command-check))

(defun c:OPORCLEAN ()
  (opor-run-safe 'opor-command-clean))

(defun c:OPORDUMP ()
  (opor-run-safe 'opor-dump-created))

(defun c:OPORTABLES ()
  (opor-run-safe 'opor-dump-all-total-tables))

(defun c:OPOROLD ()
  (opor-run-safe 'opor-dump-old-near-session))

(defun c:OPORSHOW ()
  (opor-run-safe 'opor-show-handle))

(defun c:OPORDEBUG ()
  (opor-debug-session)
  (princ))

(opor-log "Загружено. Команды: OPOR, XX, OPORTIN, OPORSLOPE, OPORSLOPEWR, OPORHEIGHTCHECK, OPORWRITELEVEL, OPORAUTOLEVEL, OPORRING, OPORREGCHECK, OPORCHECK, OPORCLEAN, OPORDUMP, OPORTABLES, OPOROLD, OPORSHOW, OPORDEBUG.")
(princ)
