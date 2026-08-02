;;; OPOR support insertion for constant-height MVP

(defun opor-support-block-name (line)
  (cdr (assoc line *opor-block-by-line*)))

(defun opor-point-key (pt tol)
  (list
    (opor-round (/ (car pt) tol))
    (opor-round (/ (cadr pt) tol))
    (opor-round (/ (caddr pt) tol))))

(defun opor-unique-points (points tol / seen result key)
  (setq seen '())
  (setq result '())
  (foreach pt points
    (setq key (opor-point-key pt tol))
    (if (not (member key seen))
      (progn
        (setq seen (cons key seen))
        (setq result (cons (opor-2d pt) result)))))
  (reverse result))

(defun opor-point-near-any-p (pt points tol / found)
  (setq found nil)
  (foreach other points
    (if (and (not found) (< (distance (opor-2d pt) (opor-2d other)) tol))
      (setq found T)))
  found)

(defun opor-remove-points-near (points compare tol / result)
  (setq result '())
  (foreach pt points
    (if (not (opor-point-near-any-p pt compare tol))
      (setq result (cons pt result))))
  (reverse result))

(defun opor-hole-vertex-points (holes / points)
  (setq points '())
  (foreach hole holes
    (setq points (append points (opor-polyline-vertices hole))))
  points)

(defun opor-support-set-first-attribute (block text / raw atts)
  (setq raw (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (if (not (vl-catch-all-error-p raw))
    (progn
      (setq atts (opor-variant-list raw))
      (if atts
        (vla-put-TextString (car atts) text)))))

(defun opor-support-insert-with (pt color text / line block-name scale block)
  (setq line (opor-session-get 'line))
  (setq block-name (opor-support-block-name line))
  (setq scale (/ (opor-session-get 'radius) 100.0))
  (if (and block-name (opor-block-exists-p block-name))
    (progn
      (setq block
        (vla-InsertBlock
          (opor-ms)
          (vlax-3d-point pt)
          block-name
          scale scale 1.0 0.0))
      (vla-put-Layer block *opor-layer-supports*)
      (if color (vl-catch-all-apply 'vla-put-Color (list block color)))
      (opor-support-set-first-attribute block text)
      (opor-register-created block "support")
      block)
    nil))

(defun opor-support-insert (pt / color range)
  (setq color (opor-session-get 'support-color))
  (setq range (opor-session-get 'support-range))
  (opor-support-insert-with pt color range))

(defun opor-filter-working-area (points boundary holes)
  (vl-remove-if
    '(lambda (pt) (not (opor-point-in-working-area-p pt boundary holes)))
    points))

;; Три класса точек как в b2_mains: вершины контура (+вершины областей высот
;; в перем. режиме), границы (концы линий сетки), узлы (пересечения + вершины
;; проёмов). Все дедупы — допуски VBA.
(defun opor-support-point-groups (session / boundary holes grid vertices border nodes hole-vertices level-vertices raw-border-count raw-node-count after-self-node-count after-vertex-node-count)
  (setq boundary (opor-session-get 'outer-boundary))
  (setq holes (opor-session-get 'holes))
  (setq grid (opor-session-get 'grid))
  ;; VBA: equalpointsDel (0.01) всегда
  (setq vertices
    (opor-unique-points
      (opor-polyline-vertices boundary)
      *opor-vba-point-dedupe-tolerance*))
  (setq hole-vertices (opor-hole-vertex-points holes))
  (if (= (opor-session-get 'mode) "var-height")
    (progn
      (setq level-vertices (opor-session-get 'level-vertex-points))
      (if (not (listp level-vertices)) (setq level-vertices '()))
      ;; VBA: arrv + вершины областей, снова equalpointsDel (0.01)
      (setq vertices
        (opor-unique-points
          (append vertices level-vertices)
          *opor-vba-point-dedupe-tolerance*))))
  (setq border (cdr (assoc 'endpoint-points grid)))
  (setq raw-border-count (length border))
  (setq border
    (opor-remove-points-near
      border
      vertices
      *opor-vba-vertex-border-tolerance*))
  (setq border (opor-unique-points border *opor-support-dedupe-tolerance*))
  (setq nodes (cdr (assoc 'node-points grid)))
  (setq nodes (append nodes hole-vertices))
  (setq raw-node-count (length nodes))
  (setq nodes (opor-unique-points nodes *opor-vba-node-self-dedupe-tolerance*))
  (setq after-self-node-count (length nodes))
  (setq nodes
    (opor-remove-points-near
      nodes
      vertices
      *opor-support-dedupe-tolerance*))
  (setq after-vertex-node-count (length nodes))
  (setq nodes
    (opor-remove-points-near
      nodes
      border
      *opor-support-dedupe-tolerance*))
  (setq vertices (opor-filter-working-area vertices boundary holes))
  (setq border (opor-filter-working-area border boundary holes))
  (setq nodes (opor-filter-working-area nodes boundary holes))
  (opor-session-set 'support-vertex-count (length vertices))
  (opor-session-set 'support-raw-border-count raw-border-count)
  (opor-session-set 'support-border-count (length border))
  (opor-session-set 'support-raw-node-count raw-node-count)
  (opor-session-set 'support-after-self-node-count after-self-node-count)
  (opor-session-set 'support-after-vertex-node-count after-vertex-node-count)
  (opor-session-set 'support-node-count (length nodes))
  (list
    (cons 'vertices vertices)
    (cons 'border border)
    (cons 'nodes nodes)))

(defun opor-support-points (session / groups)
  (setq groups (opor-support-point-groups session))
  (opor-unique-points
    (append
      (cdr (assoc 'vertices groups))
      (cdr (assoc 'border groups))
      (cdr (assoc 'nodes groups)))
    *opor-support-dedupe-tolerance*))

(defun opor-supports-place (session / points blocks block)
  (setq points (opor-support-points session))
  (setq blocks '())
  (foreach pt points
    (setq block (opor-support-insert pt))
    (if block (setq blocks (cons block blocks))))
  (setq blocks (reverse blocks))
  (opor-session-set 'support-blocks blocks)
  (opor-session-set 'support-count (length blocks))
  blocks)

(princ)
