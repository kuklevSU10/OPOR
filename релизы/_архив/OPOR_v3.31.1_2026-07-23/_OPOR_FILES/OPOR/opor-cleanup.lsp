;;; OPOR object marking and cleanup

(defun opor-current-session-id ()
  (if (and (boundp '*opor-session*) *opor-session*)
    (opor-session-get 'session-id)
    "no-session"))

(defun opor-object-ename (obj)
  (cond
    ((= (type obj) 'ENAME) obj)
    ((= (type obj) 'VLA-OBJECT)
      (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
    (t nil)))

(defun opor-mark-object (obj object-type / en old xdata result session-id)
  (if obj
    (progn
      (regapp *opor-xdata-app*)
      (setq en (opor-object-ename obj))
      (if (and en (not (vl-catch-all-error-p en)))
        (progn
          (setq old (entget en))
          (setq session-id (opor-current-session-id))
          (setq xdata
            (list
              (list -3
                (list *opor-xdata-app*
                  (cons 1000 (opor-string object-type))
                  (cons 1000 *opor-version*)
                  (cons 1000 (opor-string session-id))))))
          (setq result (vl-catch-all-apply 'entmod (list (append old xdata))))
          (if (vl-catch-all-error-p result)
            (opor-log (strcat "XData не записана: " (vl-catch-all-error-message result))))))))
  obj)

(defun opor-delete-object (obj)
  (cond
    ((= (type obj) 'VLA-OBJECT)
      (vl-catch-all-apply 'vla-Delete (list obj)))
    ((and (= (type obj) 'ENAME) (entget obj))
      (vl-catch-all-apply 'entdel (list obj))))
  nil)

(defun opor-object-live-p (obj / result)
  (cond
    ((= (type obj) 'ENAME) (if (entget obj) T nil))
    ((= (type obj) 'VLA-OBJECT)
      (setq result
        (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
      (not (vl-catch-all-error-p result)))
    (t nil)))

(defun opor-clean-session (/ objects count)
  (setq objects (opor-session-get 'created-objects))
  (setq count 0)
  (foreach obj objects
    (if (opor-object-live-p obj)
      (progn
        (opor-delete-object obj)
        (setq count (1+ count)))))
  (opor-session-set 'created-objects '())
  count)

(defun opor-object-has-opor-xdata-p (obj / en data)
  (setq en (vlax-vla-object->ename obj))
  (if en
    (progn
      (setq data (entget en (list *opor-xdata-app*)))
      (if (assoc -3 data) T nil))
    nil))

(defun opor-clean-xdata (/ objects obj count)
  (setq objects '())
  (setq count 0)
  (vlax-for obj (opor-ms)
    (if (opor-object-has-opor-xdata-p obj)
      (setq objects (cons obj objects))))
  (foreach obj objects
    (opor-delete-object obj)
    (setq count (1+ count)))
  count)

(defun opor-command-clean (/ mode count)
  (initget "Session All")
  (setq mode (getkword "\nОчистка OPOR [Session/All] <All>: "))
  (if (not mode) (setq mode "All"))
  (if (= mode "Session")
    (setq count (opor-clean-session))
    (setq count (opor-clean-xdata)))
  (opor-log (strcat "Удалено объектов: " (itoa count)))
  (princ))

(princ)
