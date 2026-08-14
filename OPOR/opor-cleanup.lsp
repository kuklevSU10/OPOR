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
          (if (or (vl-catch-all-error-p result)
                  (not result)
                  (not (opor-object-has-opor-xdata-p obj)))
            (progn
              (opor-log
                (strcat "XData не записана"
                  (if (vl-catch-all-error-p result)
                    (strcat ": " (vl-catch-all-error-message result))
                    ".")))
              (setq obj nil)))))))
  obj)

(defun opor-delete-object (obj / result)
  (if (not (opor-object-live-p obj))
    T
    (progn
      (setq result
        (cond
          ((= (type obj) 'VLA-OBJECT)
            (vl-catch-all-apply 'vla-Delete (list obj)))
          ((= (type obj) 'ENAME)
            (vl-catch-all-apply 'entdel (list obj)))
          (t nil)))
      (and (not (vl-catch-all-error-p result))
           (not (opor-object-live-p obj))))))

(defun opor-object-live-p (obj / result)
  (cond
    ((= (type obj) 'ENAME) (if (entget obj) T nil))
    ((= (type obj) 'VLA-OBJECT)
      (setq result
        (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
      (not (vl-catch-all-error-p result)))
    (t nil)))

(defun opor-object-xdata-info (obj / en data app values)
  (setq en (opor-object-ename obj))
  (if (and en (not (vl-catch-all-error-p en)))
    (progn
      (setq data (entget en (list *opor-xdata-app*)))
      (setq app (cadr (assoc -3 data)))
      (if (and app (= (car app) *opor-xdata-app*))
        (progn
          (setq values
            (vl-remove-if-not
              '(lambda (item) (and (listp item) (= (car item) 1000)))
              (cdr app)))
          (list
            (cons 'type (if (nth 0 values) (cdr (nth 0 values)) "unknown"))
            (cons 'version (if (nth 1 values) (cdr (nth 1 values)) "unknown"))
            (cons 'session (if (nth 2 values) (cdr (nth 2 values)) "legacy"))))
        nil))
    nil))

(defun opor-xdata-session-ids (/ obj info sid sessions)
  (setq sessions '())
  (vlax-for obj (opor-ms)
    (setq info (opor-object-xdata-info obj))
    (if info
      (progn
        (setq sid (cdr (assoc 'session info)))
        (if (not (member sid sessions))
          (setq sessions (cons sid sessions))))))
  (acad_strlsort sessions))

(defun opor-latest-xdata-session-id (/ sessions regular)
  (setq sessions (opor-xdata-session-ids))
  ;; Старые объекты могли иметь служебные session-id legacy/no-session.
  ;; Они не должны выигрывать сортировку у нормального timestamp-id.
  (setq regular
    (vl-remove-if
      '(lambda (sid) (member sid '("legacy" "no-session")))
      sessions))
  (if regular
    (car (reverse regular))
    (if sessions (car (reverse sessions)) nil)))

(defun opor-xdata-objects-for-session (session-id / objects obj info)
  (setq objects '())
  (if session-id
    (vlax-for obj (opor-ms)
      (setq info (opor-object-xdata-info obj))
      (if (and info (= (cdr (assoc 'session info)) session-id))
        (setq objects (cons obj objects)))))
  objects)

(defun opor-clean-session (/ refs live-refs current-id cleaned-id target-id objects obj count failed)
  ;; created-objects живёт только в памяти. После повторной загрузки LISP или
  ;; прежнего входа OPOR -> Clean список уже пуст, хотя XData в DWG сохранена.
  ;; Поэтому удаляем всю последнюю XData-сессию, а не только оставшиеся VLA-ссылки.
  (setq refs (if (and (boundp '*opor-session*) *opor-session*)
               (opor-session-get 'created-objects)
               nil))
  (setq live-refs
    (vl-remove-if-not 'opor-object-live-p refs))
  (setq current-id (opor-current-session-id))
  (setq cleaned-id
    (if (and (boundp '*opor-session*) *opor-session*)
      (opor-session-get 'cleaned-session-id)
      nil))
  (setq target-id
    (cond
      (live-refs current-id)
      (cleaned-id cleaned-id)
      (t (opor-latest-xdata-session-id))))
  (setq objects (opor-xdata-objects-for-session target-id))
  (setq count 0 failed '())
  (foreach obj objects
    (if (opor-object-live-p obj)
      (if (opor-delete-object obj)
        (setq count (1+ count))
        (setq failed (cons obj failed)))))
  (if (and (boundp '*opor-session*) *opor-session*)
    (progn
      (if (= target-id current-id)
        (opor-session-set 'created-objects (reverse failed)))
      (opor-session-set 'cleaned-session-id target-id)))
  (if target-id
    (opor-log (strcat "OPORCLEAN Session: сессия=" target-id ".")))
  (if failed
    (opor-log
      (strcat "OPORCLEAN Session: не удалось удалить объектов="
        (itoa (length failed)) ".")))
  count)

(defun opor-object-has-opor-xdata-p (obj / en data)
  (setq en (opor-object-ename obj))
  (if (and en (not (vl-catch-all-error-p en)))
    (progn
      (setq data (entget en (list *opor-xdata-app*)))
      (if (assoc -3 data) T nil))
    nil))

(defun opor-clean-xdata (/ objects obj count failed)
  (setq objects '())
  (setq count 0 failed 0)
  (vlax-for obj (opor-ms)
    (if (opor-object-has-opor-xdata-p obj)
      (setq objects (cons obj objects))))
  (foreach obj objects
    (if (opor-delete-object obj)
      (setq count (1+ count))
      (setq failed (1+ failed))))
  (if (> failed 0)
    (opor-log
      (strcat "OPORCLEAN All: не удалось удалить объектов="
        (itoa failed) ".")))
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
