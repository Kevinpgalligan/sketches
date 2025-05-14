(in-package sketches)

;; A barebones implementation of 2d Verlet physics.
;;
;; TODO:
;;    -1. improve interface for accessing particle coordinates.
;;        (need to access 2 at once for constraints).
;;        maybe: (with-particle-xy (x1 y1 x2 y2)
;;                                 (p1-id p2-id)
;;                                 world
;;                 [do-stuff])
;;    0. finish stick & spring constraints.
;;       maybe "not closer than" and "not further than" constraints, also?
;;    1. fixing points.
;;    2. manually apply forces to points.
;;    3. reimplement toast sketch.
;;    4. (unrelated) fix space invaders sketch, if broken.
;;    5. could short-circuit verlet steps based on change in position.
;;
;; References:
;;   "Advanced Character Physics", Thomas Jakobsen (*** essential reading ***):
;;     https://www.cs.cmu.edu/afs/cs/academic/class/15462-s13/www/lec_slides/Jakobsen.pdf
;;   Coding Train soft body physics:
;;     https://www.youtube.com/watch?v=IxdGyqhppis
;;   Jelly car explainer:
;;     https://www.youtube.com/watch?v=3OmkehAJoyo

(defclass world ()
  ((particles
    :initform (make-array 0 :fill-pointer t :adjustable t)
    :accessor particles)
   (forces
    :initform (make-array 0 :fill-pointer t :adjustable t)
    :accessor forces)
   (gravity
    :initarg :gravity
    :initform nil
    :accessor gravity)
   (constraints
    :initform nil
    :accessor constraints)
   (verlet-iterations
    :initarg :verlet-iterations
    :initform 1
    :accessor verlet-iterations)))

(defun make-world (&key gravity)
  "Returns a new instance of a physics world."
  (let ((w (make-instance 'world)))
    (when gravity
      (enable-gravity w gravity))
    w))

(defun enable-gravity (world a)
  "Adds gravity to WORLD with acceleration A (a non-negative number)."
  (when (< a 0)
    (error "Gravity must be non-negative"))
  (setf (gravity world) a))

(defun disable-gravity (world)
  "Turns off gravity in WORLD."
  (setf (gravity world) nil))

(defun add-constraint (world constraint)
  (push constraint (constraints world)))

(defclass verlet-particle ()
  ((pos
    :initarg :pos
    :accessor pos)
   (old-pos
    :initarg :old-pos
    :accessor old-pos)))

(defun make-verlet-particle (&key (pos (vec2 0 0)))
  (make-instance 'verlet-particle :pos pos :old-pos (v-copy pos)))

(defun add-particle (world &key (x 0) (y 0))
  "Adds a particle to WORLD and returns its ID."
  (vector-push-extend (make-verlet-particle :pos (vec2 x y)) (particles world))
  (vector-push-extend (vec2 0 0) (forces world))
  (1- (length (particles world))))

(defun get-particle-xy (world particle-id)
  (aref (particles world) particle-id))

(defmacro with-particle-xy ((xvar yvar) world particle-id &body body)
  (let ((posvar (gensym)))
    `(let* ((,posvar (pos (get-particle ,world ,particle-id)))
            (,xvar (vx ,posvar))
            (,yvar (vy ,posvar)))
       ,@body)))

(defun update-world (world)
  "Perform a physics step in WORLD, updating its particles."
  (accumulate-forces world)
  (verlet-step world)
  (apply-constraints world))

(defun accumulate-forces (world)
  (when (gravity world)
    (loop for a across (forces world)
          do (decf (vy a) (gravity world)))))

(defun verlet-step (world)
  (loop for p across (particles world)
        for a across (forces world)
        do (update-particle! p a)
        ;; Reset accumulated forces for each particle.
        do (setf (vx a) 0
                 (vy a) 0)))

(defun update-particle! (p a)
  ;; Formula from Advanced Character Physics:
  ;;    x' <- 2x - x* + a
  ;;    x* <- x
  ;; ...where x is the current position, x* is the old position, x' is
  ;; the new position, and a is the acceleration from all the accumulated forces.
  (with-accessors ((x pos)
                   (oldx old-pos))
      p
    (rotatef x oldx)
    (v-scale! -1 x)
    (v+! x oldx)
    (v+! x oldx) ; twice gives 2x, not ideal from an effiency perspective
    (v+! x a)))

(defgeneric apply-constraint (world constraint))

(defclass bounds-constraint ()
  ((min-x
    :initarg :min-x
    :accessor min-x)
   (max-x
    :initarg :max-x
    :accessor max-x)
   (min-y
    :initarg :min-y
    :accessor min-y)
   (max-y
    :initarg :max-y
    :accessor max-y)))

(defclass stick-constraint ()
  ((p1-id
    :initarg :p1-id
    :accessor p1-id)
   (p2-id
    :initarg :p2-id
    :accessor p2-id)
   (len
    :initarg :len
    :accessor len)))

(defclass spring-constraint ()
  ((p1-id
    :initarg :p1-id
    :accessor p1-id)
   (p2-id
    :initarg :p2-id
    :accessor p2-id)
   (len
    :initarg :len
    :accessor len)
   (coeff
    :initarg :coeff
    :accessor coeff)))

(defmethod apply-constraint (world (c bounds-constraint))
  (loop for particle across (particles world)
        for p = (pos particle)
        do (setf (vx p) (max (min-x c) (min (vx p) (max-x c)))
                 (vy p) (max (min-y c) (min (vy p) (max-y c))))))

(defmethod apply-constraint (world (c stick-constraint))
  (apply-spring-constraint world (p1-id c) (p2-id c) (len c) 1))

(defmethod apply-constraint (world (c spring-constraint))
  (apply-spring-constraint world (p1-id c) (p2-id c) (len c) (coeff c)))

(defun apply-spring-constraint (world p1-id p2-id len coeff)
  ;; TODO
  )

(defun apply-constraints (world)
  (loop repeat (verlet-iterations world)
        do (map nil
                (lambda (constraint)
                  (apply-constraint world constraint))
                (constraints world))))

(defun add-bounds (world max-x max-y &key (min-x 0) (min-y 0))
  (add-constraint world
                  (make-instance 'bounds-constraint
                                 :min-x min-x
                                 :max-x max-x
                                 :min-y min-y
                                 :max-y max-y)))
