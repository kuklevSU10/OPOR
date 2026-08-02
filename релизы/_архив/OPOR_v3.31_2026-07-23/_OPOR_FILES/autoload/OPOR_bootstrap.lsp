;;; OPOR Autodesk bundle bootstrap.
;;; The installer places this file in a trusted ApplicationPlugins bundle.

(vl-load-com)

(defun opor-bundle-install-root (/ override program-files)
  (setq override (getenv "OPOR_BUNDLE_ROOT"))
  (if (and override (/= override ""))
    (vl-string-right-trim "\\/" override)
    (progn
      (setq program-files (getenv "ProgramFiles"))
      (if program-files
        (strcat (vl-string-right-trim "\\/" program-files)
                "\\Autodesk\\ApplicationPlugins\\OPOR.bundle")
        nil))))

(setq *opor-bundle-root* (opor-bundle-install-root))
(setq *opor-root*
  (if *opor-bundle-root*
    (strcat *opor-bundle-root* "\\Contents\\OPOR")
    nil))

(if *opor-root*
  (setq *opor-bundle-load-result*
    (vl-catch-all-apply
      'load
      (list (strcat *opor-root* "\\opor-loader.lsp")))))

(if (or (not *opor-root*)
        (vl-catch-all-error-p *opor-bundle-load-result*))
  (alert
    (strcat
      "OPOR: ошибка автоматической загрузки.\n\n"
      (if *opor-root* *opor-root* "Не найдена папка установки.")
      (if (and (boundp '*opor-bundle-load-result*)
               (vl-catch-all-error-p *opor-bundle-load-result*))
        (strcat "\n\n" (vl-catch-all-error-message *opor-bundle-load-result*))
        "")))
  (princ "\n[OPOR] Автозагрузка v3.31 выполнена. Команды: OPOR, XX."))

(princ)
