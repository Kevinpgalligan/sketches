;;;; Press spacebar to refresh.
;;;; 
;;;; References:
;;;;   https://thecodingtrain.com/challenges/14-fractal-trees-recursive
;;;;   https://thecodingtrain.com/challenges/15-object-oriented-fractal-trees
;;;;   https://thecodingtrain.com/challenges/16-l-system-fractal-trees

(sketches:def-sketch-package trees.recursive)
(in-package kg.sketch.trees.recursive)

(defsketch trees
    ((pal (get-palette :sadiq))
     (initial-height (/ height 4))
     (min-size (/ height 60))
     (shrink-ratio 0.6)
     (theta 25))
  (shuffle-palette pal)
  (background (next-colour pal))
  (translate (/ width 2) height)
  (with-pen (make-pen :stroke (next-colour pal) :weight 2)
    (draw-branch initial-height min-size theta shrink-ratio))
  (setf initial-height (+ (/ height 7) (random (float (/ height 7)))))
  (setf min-size (+ (/ height 80) (random (float (/ height 60)))))
  (setf theta (+ 10 (random 30)))
  (setf shrink-ratio (+ 0.5 (random 0.2)))
  (stop-loop))

(defmethod on-key ((instance trees) (key (eql :space)) (state (eql :keyup)))
  (start-loop))

(defun draw-branch (h min-size theta shrink-ratio)
  (when (not (< h min-size))
    (line 0 0 0 (- h))
    (with-current-matrix
      (translate 0 (- h))
      (rotate (- theta))
      (draw-branch (* h shrink-ratio) min-size theta shrink-ratio))
    (with-current-matrix
      (translate 0 (- h))
      (rotate theta)
      (draw-branch (* h shrink-ratio) min-size theta shrink-ratio))))

(sketches:def-sketch-package trees.oo)
(in-package kg.sketch.trees.oo)

;; Copying a lot of code here, ~sketch~ doesn't yet have an
;; interface for sharing behaviour between sketches.
(defsketch trees
    ((pal (get-palette :sadiq))
     (initial-height (/ height 4))
     (min-size (/ height 60))
     (shrink-ratio 0.6)
     (theta 25)
     (leaf (load-static-resource "leaf.png")))
  (shuffle-palette pal)
  (background (next-colour pal))
  (with-pen (make-pen :stroke (next-colour pal) :weight 2)
    (let ((tree (make-instance 'branch
                               :begin (vec2 (/ width 2) height)
                               :end (vec2 (/ width 2) (- height initial-height)))))
      (expand-tree tree min-size theta shrink-ratio)
      (show-tree tree leaf)))
  (setf initial-height (+ (/ height 5) (random (float (/ height 4)))))
  (setf min-size (+ (/ height 80) (random (float (/ height 30)))))
  (setf theta (+ 10 (random 30)))
  (setf shrink-ratio (+ 0.5 (random 0.2)))
  (stop-loop))

(defmethod on-key ((instance trees) (key (eql :space)) (state (eql :keyup)))
  (start-loop))

(defclass branch ()
  ((begin :initarg :begin)
   (end :initarg :end)
   (children :initform nil)))

(defun expand-tree (tree min-size theta shrink-ratio)
  (with-slots (begin end children) tree
    (let* ((dir (v- end begin))
          (h (* shrink-ratio (v-length dir))))
      (loop for angle-fun in (list #'+ #'-)
            do (push (make-instance
                      'branch
                      :begin end
                      :end (v+ end
                               (v-rescale h
                                          (v-rotate (funcall angle-fun (radians theta))
                                                    dir))))
                     children))
      (when (> h min-size)
        (loop for child in children
              do (expand-tree child min-size theta shrink-ratio))))))

(defun show-tree (tree leaf)
  (with-slots (begin end children) tree
    (line (vx begin) (vy begin) (vx end) (vy end))
    (if children
        (loop for child in children
              do (show-tree child leaf))
        (with-current-matrix
          (translate (vx end) (vy end))
          (rotate (random 360.0))
          (draw leaf :x (random-leaf-coord)
                     :y (random-leaf-coord))))))

(defun random-leaf-coord ()
  (* 10 (- 1 (random 2.0))))


(sketches:def-sketch-package trees.lsystem)
(in-package kg.sketch.trees.lsystem)

(def-lsystem tree
    ((#\F "FF+[+F-F-F]-[-F+F+F]")))

(defsketch trees
    ((pal (get-palette :sadiq))
     (theta 25)
     ;; Getting a memory error when rounds > 5.
     (rounds 5))
  (shuffle-palette pal)
  (background (next-colour pal))
  (with-pen (make-pen :stroke (next-colour pal) :weight 2)
    (let ((instructions "F")
          (branch-length height))
      (loop repeat rounds
            do (execute-instructions instructions width height branch-length theta)
            do (setf instructions (evaluate-lsystem 'tree :axiom instructions))
            do (setf branch-length (* 0.5 branch-length)))))
  (setf theta (+ 10 (random 30)))
  (stop-loop))

(defmethod on-key ((instance trees) (key (eql :space)) (state (eql :keyup)))
  (start-loop))

(defun execute-instructions (ins width height branch-length theta)
  (with-current-matrix
      (translate (/ width 2) height)
    (loop for c across ins
          do (case c
               (#\F
                (line 0 0 0 (- branch-length))
                (translate 0 (- branch-length)))
               (#\+ (rotate theta))
               (#\- (rotate (- theta)))
               (#\[ (push-matrix))
               (#\] (pop-matrix))))))
