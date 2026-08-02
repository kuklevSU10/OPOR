;;; OPOR logging, errors and command safety

(defun opor-doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun opor-ms ()
  (vla-get-ModelSpace (opor-doc)))

(setq *opor-log-file* nil)

(defun opor-log-trim-dir (dir)
  (if dir (vl-string-right-trim "\\/" dir) nil))

(defun opor-log-directory (/ override local-root opor-root log-root)
  (setq override (getenv "OPOR_LOG_ROOT"))
  (if (and override (/= override ""))
    (progn
      (setq log-root (opor-log-trim-dir override))
      (if (not (vl-file-directory-p log-root))
        (vl-catch-all-apply 'vl-mkdir (list log-root))))
    (progn
      (setq local-root (getenv "LOCALAPPDATA"))
      (if (and local-root (/= local-root ""))
        (progn
          (setq opor-root (strcat (opor-log-trim-dir local-root) "\\OPOR"))
          (if (not (vl-file-directory-p opor-root))
            (vl-catch-all-apply 'vl-mkdir (list opor-root)))
          (setq log-root (strcat opor-root "\\logs"))
          (if (not (vl-file-directory-p log-root))
            (vl-catch-all-apply 'vl-mkdir (list log-root)))))))
  (if (and log-root (vl-file-directory-p log-root)) log-root nil))

(defun opor-log-path (/ dir day)
  (if (and *opor-log-file* (findfile *opor-log-file*))
    *opor-log-file*
    (if (setq dir (opor-log-directory))
      (progn
        (setq day (itoa (fix (getvar "CDATE"))))
        (setq *opor-log-file* (strcat dir "\\OPOR_" day ".log"))))))

(defun opor-log-write (msg / path stream)
  (if (setq path (opor-log-path))
    (progn
      (setq stream (vl-catch-all-apply 'open (list path "a")))
      (if (and stream (not (vl-catch-all-error-p stream)))
        (progn
          (write-line
            (strcat (rtos (getvar "CDATE") 2 6) " | " msg)
            stream)
          (close stream)
          T)))))

(defun opor-log (msg / text)
  (setq text (if msg (vl-princ-to-string msg) ""))
  (princ (strcat "\n[OPOR] " text))
  (opor-log-write text)
  text)

(defun opor-alert (msg)
  (opor-log (strcat "ALERT | " msg))
  (alert (strcat "OPOR\n\n" msg)))

(defun opor-string (value)
  (if value (vl-princ-to-string value) ""))

(defun opor-join-lines (items / out)
  (setq out "")
  (foreach item items
    (setq out (strcat out "\n- " (opor-string item))))
  out)

(defun opor-round (value)
  (if (< value 0.0)
    (- (fix (+ (- value) 0.5)))
    (fix (+ value 0.5))))

;; VBA Round/CLng: банковское округление, 0.5 уходит к чётному
(defun opor-round-half-even (value / low frac)
  (setq low (fix value))
  (if (and (< value 0.0) (/= value low)) (setq low (1- low)))
  (setq frac (- value low))
  (cond
    ((< frac 0.5) low)
    ((> frac 0.5) (1+ low))
    ((= 0 (rem low 2)) low)
    (t (1+ low))))

;; variant/safearray -> список; пустой массив -> '() (без "Неверный индекс")
(defun opor-variant-list (value)
  (if (= (type value) 'variant)
    (setq value (vlax-variant-value value)))
  (if (and (= (type value) 'safearray)
           (>= (vlax-safearray-get-u-bound value 1)
               (vlax-safearray-get-l-bound value 1)))
    (vlax-safearray->list value)
    '()))

(defun opor-safe-getvar (name / value)
  (setq value (vl-catch-all-apply 'getvar (list name)))
  (if (vl-catch-all-error-p value) nil value))

(defun opor-safe-setvar (name value)
  (if value
    (vl-catch-all-apply 'setvar (list name value))))

(setq *opor-safe-state* nil)
(setq *opor-old-error* nil)

(defun opor-cancel-message-p (msg)
  (and msg
       (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*ПРЕРВАН*,*ОТМЕН*")))

(defun opor-safe-restore-visuals ()
  (vl-catch-all-apply 'opor-layers-restore nil)
  (vl-catch-all-apply 'opor-unhighlight-session nil)
  (vl-catch-all-apply 'opor-view-restore nil)
  (princ))

(defun opor-safe-begin (/ doc)
  (vl-load-com)
  (setq doc (opor-doc))
  (setq *opor-safe-state*
    (list
      (cons 'doc doc)
      (cons 'vars
        (list
          (cons "OSMODE" (opor-safe-getvar "OSMODE"))
          (cons "CMDECHO" (opor-safe-getvar "CMDECHO"))
          (cons "CLAYER" (opor-safe-getvar "CLAYER"))
          (cons "FILEDIA" (opor-safe-getvar "FILEDIA"))))))
  (vl-catch-all-apply 'vla-StartUndoMark (list doc))
  (opor-safe-setvar "CMDECHO" 0)
  (princ))

(defun opor-safe-restore (/ doc vars)
  (if *opor-safe-state*
    (progn
      (setq doc (cdr (assoc 'doc *opor-safe-state*)))
      (setq vars (cdr (assoc 'vars *opor-safe-state*)))
      (foreach pair vars
        (opor-safe-setvar (car pair) (cdr pair)))
      (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))))
  (setq *opor-safe-state* nil)
  (princ))

(defun opor-error-handler (msg)
  (opor-safe-restore-visuals)
  (opor-safe-restore)
  (setq *error* *opor-old-error*)
  (if msg
    (if (opor-cancel-message-p msg)
      (opor-log (strcat "CANCEL | " msg))
      (opor-log (strcat "ERROR | " msg))))
  (princ))

(defun opor-run-safe (fn / result msg)
  (opor-log
    (strcat
      "START | " (vl-princ-to-string fn)
      " | DWG=" (opor-string (opor-safe-getvar "DWGNAME"))
      " | ACADVER=" (opor-string (opor-safe-getvar "ACADVER"))))
  (setq *opor-old-error* *error*)
  (opor-safe-begin)
  (setq *error* (lambda (msg) (opor-error-handler msg)))
  (setq result (vl-catch-all-apply fn nil))
  (opor-safe-restore-visuals)
  (opor-safe-restore)
  (setq *error* *opor-old-error*)
  (if (vl-catch-all-error-p result)
    (progn
      (setq msg (vl-catch-all-error-message result))
      (if (opor-cancel-message-p msg)
        (opor-log (strcat "CANCEL | " (vl-princ-to-string fn) " | " msg))
        (opor-log (strcat "ERROR | " (vl-princ-to-string fn) " | " msg)))
      nil)
    (progn
      (opor-log (strcat "DONE | " (vl-princ-to-string fn)))
      result)))

(princ)
