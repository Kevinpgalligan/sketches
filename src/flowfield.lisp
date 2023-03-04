;; References:
;;   Coding Train, https://www.youtube.com/watch?v=BjoM9oKOAKY
;;     (WATCHED, there are some useful comments such as how to
;;      make the particles more liquid-like and how to optimise)
;;   Tyler Hobbs article, https://tylerxhobbs.com/essays/2020/flow-fields
;;   Steve's Makerspace, https://www.youtube.com/watch?v=oKwi8h_yTsY&list=LL&index=20&t=2s

(in-package sketches)

(defclass particle ()
  ((pos :initarg :pos :accessor pos)
   (velocity :initarg :velocity :accessor velocity)
   (prev-position :initarg :prev-position :accessor prev-position)))

(defun make-particle (x y)
  (let ((initial-position (vec2 x y)))
    (make-instance 'particle
                   :pos initial-position
                   :velocity (vec2 0 0)
                   :prev-position initial-position)))

(defun get-nearest-flow-point (v spacing)
  (vec2 (round-to-nearest-multiple (vx v) spacing)
        (round-to-nearest-multiple (vy v) spacing)))

(defun get-pos-on-unit-circle (angle-radians)
  (vec2 (cos angle-radians) (sin angle-radians)))

(defsketch flowfield
    ((width 800)
     (height 600)
     (copy-pixels t)
     (rng (random-state:make-generator 'random-state:mersenne-twister-64))
     (particles (loop repeat 100
                      collect (make-particle (random-state:random-int rng 0 width)
                                             (random-state:random-int rng 0 height))))
     (colour (rgb 0 0 0 0.1))
     (pen (make-pen :stroke colour :fill colour :weight 1))
     (flow-strength 0.5)
     (flow-spacing 50)
     (max-velocity 5)
     (N (make-vnoise)))
  (with-pen pen
    (loop for particle in particles
          do (apply #'line (concatenate 'list (pos particle) (prev-position particle)))
          do (setf (prev-position particle) (pos particle))
          do (let* ((flow-pos (get-nearest-flow-point (pos particle) flow-spacing))
                    (angle (* 2 pi (apply #'noise-get (cons N flow-pos))))
                    (dv (v-scale flow-strength (get-pos-on-unit-circle angle))))
               (setf (pos particle)
                     (v+ (pos particle) (velocity particle)))
               (when (or (outside-range-p 0 width (vx (pos particle)))
                         (outside-range-p 0 height(vy (pos particle))))
                 (setf (vx (pos particle)) (mod (vx (pos particle)) width)
                       (vy (pos particle)) (mod (vy (pos particle)) height)
                       ;; Edge case: don't wanna draw a line all the way across the screen.
                       (prev-position particle) (pos particle)))
               (setf (velocity particle)
                     (v-clamp max-velocity (v+ (velocity particle) dv)))))))

(defmethod setup ((instance flowfield) &key &allow-other-keys)
  (background +white+))
