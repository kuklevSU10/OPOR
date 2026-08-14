;;; OPOR — один файл для подключения.
;;;
;;; Подключите через APPLOAD именно ЭТОТ файл (и добавьте его в «Группу
;;; автозагрузки»). Больше ничего указывать не нужно: он сам определит, откуда
;;; его загрузили, найдёт рядом папку OPOR и передаст управление загрузчику.
;;;
;;; Как он себя находит. AutoLISP не сообщает файлу его путь (*load-truename* и
;;; *load-pathname* в AutoCAD не существуют), НО сам AutoCAD хранит этот путь в
;;; своём реестре APPLOAD: в «Группе автозагрузки», в истории и в последней
;;; открытой папке. Мы это читаем. Имена значений не угадываем — перебираем всё,
;;; что есть в ключе, поэтому способ не зависит от версии и локали AutoCAD.
;;; Найденная папка запоминается, и дальше всё поднимается молча.

(vl-load-com)

(setq *opor-stub-registry-key* "HKEY_CURRENT_USER\\Software\\OPOR")
(setq *opor-stub-autocad-key* "HKEY_CURRENT_USER\\Software\\Autodesk\\AutoCAD")

(defun opor-stub-reg-subkeys (key / value)
  (setq value (vl-catch-all-apply 'vl-registry-descendents (list key)))
  (if (vl-catch-all-error-p value) nil value))

(defun opor-stub-reg-valuenames (key / value)
  (setq value (vl-catch-all-apply 'vl-registry-descendents (list key T)))
  (if (vl-catch-all-error-p value) nil value))

(defun opor-stub-reg-string (key name / value)
  (setq value (vl-catch-all-apply 'vl-registry-read (list key name)))
  (if (or (vl-catch-all-error-p value) (/= (type value) 'STR) (= value ""))
    nil
    value))

;; В указанной папке ищем настоящий загрузчик: сначала в подпапке OPOR, потом
;; прямо в ней. Так подходит и папка поставки, и сама папка OPOR.
(defun opor-stub-loader-in-dir (dir / base)
  (if (and dir (= (type dir) 'STR) (/= dir ""))
    (progn
      (setq base (vl-string-right-trim "\\/" dir))
      (cond
        ((findfile (strcat base "\\OPOR\\opor-loader.lsp")))
        ((findfile (strcat base "\\opor-loader.lsp")))
        (T nil)))
    nil))

;; Тот же ключ, что у самого загрузчика: достаточно, чтобы папку один раз нашёл
;; любой из них.
(defun opor-stub-remembered-root ()
  (opor-stub-reg-string *opor-stub-registry-key* "Root"))

;; Все папки, которые AutoCAD помнит по APPLOAD, по всем версиям и профилям.
(defun opor-stub-appload-dirs (/ dirs ver loc profroot prof appload sub subkey vname value)
  (setq dirs '())
  (foreach ver (opor-stub-reg-subkeys *opor-stub-autocad-key*)
    (foreach loc (opor-stub-reg-subkeys (strcat *opor-stub-autocad-key* "\\" ver))
      (setq profroot
        (strcat *opor-stub-autocad-key* "\\" ver "\\" loc "\\Profiles"))
      (foreach prof (opor-stub-reg-subkeys profroot)
        (setq appload (strcat profroot "\\" prof "\\Dialogs\\Appload"))
        ;; «Группа автозагрузки» и история — там лежат полные пути к файлам.
        (foreach sub (list "Startup" "History")
          (setq subkey (strcat appload "\\" sub))
          (foreach vname (opor-stub-reg-valuenames subkey)
            (setq value (opor-stub-reg-string subkey vname))
            (if (and value (wcmatch (strcase value) "*.LSP"))
              (setq dirs (cons (vl-filename-directory value) dirs)))))
        ;; Последняя папка, открытая в APPLOAD.
        (setq value (opor-stub-reg-string appload "MainDialog"))
        (if value (setq dirs (cons value dirs))))))
  (reverse dirs))

;; Самый последний рубеж — если AutoCAD почему-то ничего не помнит. Принимаем
;; ЛЮБОЙ из двух файлов поставки и от его папки доискиваемся до загрузчика.
(defun opor-stub-ask-loader (/ selected)
  (setq selected
    (vl-catch-all-apply 'getfiled
      (list "OPOR: укажите 00_ЗАГРУЗИТЬ_OPOR.lsp или OPOR\\opor-loader.lsp"
            "00_ЗАГРУЗИТЬ_OPOR.lsp" "lsp" 0)))
  (if (or (vl-catch-all-error-p selected) (not selected))
    nil
    (opor-stub-loader-in-dir (vl-filename-directory selected))))

(defun opor-stub-find-loader (/ found)
  (cond
    ((setq found (findfile "OPOR\\opor-loader.lsp")) found)
    ((setq found (opor-stub-loader-in-dir (opor-stub-remembered-root))) found)
    ((setq found (vl-some 'opor-stub-loader-in-dir (opor-stub-appload-dirs)))
      found)
    ((setq found (opor-stub-loader-in-dir (getvar "DWGPREFIX"))) found)
    ((setq found (opor-stub-ask-loader)) found)
    (T nil)))

(setq *opor-stub-loader* (opor-stub-find-loader))

(if *opor-stub-loader*
  (progn
    ;; Загрузчик первым делом смотрит на *opor-root*, поэтому вопросов он уже
    ;; не задаст и сам запишет найденную папку в реестр.
    (setq *opor-root* (vl-filename-directory *opor-stub-loader*))
    (load *opor-stub-loader*))
  (alert
    (strcat
      "OPOR: не найден файл OPOR\\opor-loader.lsp.\n\n"
      "Держите этот файл рядом с папкой OPOR из поставки\n"
      "и при запросе укажите любой из этих двух файлов.")))

(princ)
