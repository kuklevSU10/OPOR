;;; OPOR pure Visual LISP line clipping, based on trim_proto.lsp

(defun opor-param-t (a b p / dx dy len2)
  (setq dx (- (car b) (car a)))
  (setq dy (- (cadr b) (cadr a)))
  (setq len2 (+ (* dx dx) (* dy dy)))
  (if (equal len2 0.0 1e-12)
    0.0
    (/ (+ (* (- (car p) (car a)) dx)
          (* (- (cadr p) (cadr a)) dy))
       len2)))

(defun opor-pt-at (a b tval)
  (list
    (+ (car a) (* tval (- (car b) (car a))))
    (+ (cadr a) (* tval (- (cadr b) (cadr a))))
    0.0))

(defun opor-sort-uniq (values / sorted result)
  (setq sorted (vl-sort values '<))
  (setq result '())
  (foreach value sorted
    (if (or (not result) (> (abs (- value (car result))) 1e-7))
      (setq result (cons value result))))
  (reverse result))

(defun opor-make-line (a b layer object-type / line)
  (setq line (vla-AddLine (opor-ms) (vlax-3d-point a) (vlax-3d-point b)))
  (if layer (vla-put-Layer line layer))
  (opor-register-created line object-type))

(defun opor-trim-line-by-boundaries (line boundary holes object-type / a b layer pts params made t0 t1 mid new)
  (setq a (opor-curve-start line))
  (setq b (opor-curve-end line))
  (setq layer (vla-get-Layer line))
  (setq pts (opor-obj-intersections line boundary))
  (foreach hole holes
    (setq pts (append pts (opor-obj-intersections line hole))))
  (setq params
    (opor-sort-uniq
      (cons 0.0
        (cons 1.0
          (mapcar '(lambda (pt) (opor-param-t a b pt)) pts)))))
  (setq made '())
  (while (cdr params)
    (setq t0 (car params))
    (setq t1 (cadr params))
    (if (> (- t1 t0) 1e-7)
      (progn
        (setq mid (opor-pt-at a b (/ (+ t0 t1) 2.0)))
        (if (opor-point-in-working-area-p mid boundary holes)
          (progn
            (setq new (opor-make-line (opor-pt-at a b t0) (opor-pt-at a b t1) layer object-type))
            (setq made (cons new made))))))
    (setq params (cdr params)))
  (vl-catch-all-apply 'vla-Delete (list line))
  (reverse made))

(defun opor-trim-lines-by-boundaries (lines boundary holes object-type / result)
  (setq result '())
  (foreach line lines
    (setq result (append result (opor-trim-line-by-boundaries line boundary holes object-type))))
  result)

(princ)
