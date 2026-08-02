;;; OPOR Autodesk bundle bootstrap.
;;; The installer places this file in a trusted ApplicationPlugins bundle.

(vl-load-com)

(defun opor-bundle-trim-dir (dir)
  (if dir (vl-string-right-trim "\\/" dir) nil))

(defun opor-bundle-self-root (/ source)
  (setq source
    (cond
      ((and (boundp '*load-truename*) *load-truename*) *load-truename*)
      ((and (boundp '*load-pathname*) *load-pathname*) *load-pathname*)
      (t nil)))
  (if source
    (vl-filename-directory (vl-filename-directory source))
    nil))

(defun opor-bundle-loader-exists-p (root)
  (and root
       (/= root "")
       (findfile
         (strcat (opor-bundle-trim-dir root)
                 "\\Contents\\OPOR\\opor-loader.lsp"))))

(defun opor-bundle-add-candidate (root candidates)
  (setq root (opor-bundle-trim-dir root))
  (if (and root (/= root "") (not (member root candidates)))
    (append candidates (list root))
    candidates))

(defun opor-bundle-standard-candidates (/ candidates base variable)
  (setq candidates '())
  (foreach variable (list "ProgramFiles(x86)" "ProgramFiles" "ProgramData" "APPDATA")
    (if (setq base (getenv variable))
      (setq candidates
        (opor-bundle-add-candidate
          (strcat (opor-bundle-trim-dir base)
                  "\\Autodesk\\ApplicationPlugins\\OPOR.bundle")
          candidates))))
  candidates)

(defun opor-bundle-log-directory (/ override local-root opor-root log-root)
  (setq override (getenv "OPOR_LOG_ROOT"))
  (if (and override (/= override ""))
    (progn
      (setq log-root (opor-bundle-trim-dir override))
      (if (not (vl-file-directory-p log-root))
        (vl-catch-all-apply 'vl-mkdir (list log-root))))
    (progn
      (setq local-root (getenv "LOCALAPPDATA"))
      (if (and local-root (/= local-root ""))
        (progn
          (setq opor-root (strcat (opor-bundle-trim-dir local-root) "\\OPOR"))
          (if (not (vl-file-directory-p opor-root))
            (vl-catch-all-apply 'vl-mkdir (list opor-root)))
          (setq log-root (strcat opor-root "\\logs"))
          (if (not (vl-file-directory-p log-root))
            (vl-catch-all-apply 'vl-mkdir (list log-root)))))))
  (if (and log-root (vl-file-directory-p log-root)) log-root nil))

(defun opor-bundle-log (msg / dir path stream)
  (if (setq dir (opor-bundle-log-directory))
    (progn
      (setq path
        (strcat dir "\\autoload_" (itoa (fix (getvar "CDATE"))) ".log"))
      (setq stream (vl-catch-all-apply 'open (list path "a")))
      (if (and stream (not (vl-catch-all-error-p stream)))
        (progn
          (write-line
            (strcat (rtos (getvar "CDATE") 2 6) " | " msg)
            stream)
          (close stream))))))

(defun opor-bundle-install-root (/ override self-root candidates found program-files candidate)
  (setq override (getenv "OPOR_BUNDLE_ROOT"))
  (setq self-root (opor-bundle-self-root))
  (setq candidates '())
  (if (and override (/= override ""))
    (setq candidates (opor-bundle-add-candidate override candidates)))
  (if self-root
    (setq candidates (opor-bundle-add-candidate self-root candidates)))
  (foreach candidate (opor-bundle-standard-candidates)
    (setq candidates (opor-bundle-add-candidate candidate candidates)))
  (setq found
    (vl-some
      '(lambda (candidate)
         (if (opor-bundle-loader-exists-p candidate) candidate nil))
      candidates))
  (cond
    (found found)
    ((and override (/= override "")) (opor-bundle-trim-dir override))
    ((and self-root (/= self-root "")) (opor-bundle-trim-dir self-root))
    (t
      (setq program-files (getenv "ProgramFiles"))
      (if program-files
        (strcat (opor-bundle-trim-dir program-files)
                "\\Autodesk\\ApplicationPlugins\\OPOR.bundle")
        nil))))

(setq *opor-bundle-root* (opor-bundle-install-root))
(setq *opor-root*
  (if *opor-bundle-root*
    (strcat *opor-bundle-root* "\\Contents\\OPOR")
    nil))

(opor-bundle-log
  (strcat
    "START | BUNDLE=" (if *opor-bundle-root* *opor-bundle-root* "<nil>")
    " | LOADER=" (if *opor-root* (strcat *opor-root* "\\opor-loader.lsp") "<nil>")
    " | ACADVER=" (vl-princ-to-string (getvar "ACADVER"))
    " | APPAUTOLOAD=" (vl-princ-to-string (getvar "APPAUTOLOAD"))))

(if *opor-root*
  (setq *opor-bundle-load-result*
    (vl-catch-all-apply
      'load
      (list (strcat *opor-root* "\\opor-loader.lsp")))))

(if (or (not *opor-root*)
        (vl-catch-all-error-p *opor-bundle-load-result*))
  (progn
    (opor-bundle-log
      (strcat
        "ERROR | "
        (if *opor-root* *opor-root* "Не найдена папка установки.")
        (if (and (boundp '*opor-bundle-load-result*)
                 (vl-catch-all-error-p *opor-bundle-load-result*))
          (strcat " | " (vl-catch-all-error-message *opor-bundle-load-result*))
          "")))
    (alert
      (strcat
        "OPOR: ошибка автоматической загрузки.\n\n"
        (if *opor-root* *opor-root* "Не найдена папка установки.")
        (if (and (boundp '*opor-bundle-load-result*)
                 (vl-catch-all-error-p *opor-bundle-load-result*))
          (strcat "\n\n" (vl-catch-all-error-message *opor-bundle-load-result*))
          ""))))
  (progn
    (opor-bundle-log "SUCCESS | OPOR commands loaded")
    (princ "\n[OPOR] Автозагрузка v3.31.2 выполнена. Команды: OPOR, XX, OPORLOGS.")))

(princ)
