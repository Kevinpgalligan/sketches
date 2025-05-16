(in-package sketches)

;; A barebones implementation of 2d Verlet physics.
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
   (fixed
    :initform (make-array 0 :fill-pointer t :adjustable t)
    :accessor fixed)
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

(defun fix-particle (world particle-id)
  "Opting a particle out of the physics simulation so it stays in place.
The particle is immune to all forces and ignores constraints"
  (setf (aref (fixed world) particle-id) t))

(defun unfix-particle (world particle-id)
  "Opting a particle back in to the physics simulation."
  (setf (aref (fixed world) particle-id) nil))

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

(defun add-particle (world &key (x 0) (y 0) fixed)
  "Adds a particle to WORLD and returns its ID."
  (vector-push-extend (make-verlet-particle :pos (vec2 x y)) (particles world))
  (vector-push-extend (vec2 0 0) (forces world))
  (vector-push-extend fixed (fixed world))
  (1- (length (particles world))))

(defun get-particle (world particle-id)
  (aref (particles world) particle-id))

(defun particle-position (world particle-id)
  (pos (get-particle world particle-id)))

(defmacro with-particle-xy ((xvar yvar) world particle-id &body body)
  (alexandria:with-gensyms (posvar)
    `(let* ((,posvar (pos (get-particle ,world ,particle-id)))
            (,xvar (vx ,posvar))
            (,yvar (vy ,posvar)))
       ,@body)))

(defun try-translate-particle (world particle-id diff)
  "DIFF should be a 2d vector."
  (when (not (aref (fixed world) particle-id))
    (v+! (pos (get-particle world particle-id)) diff)))

(defun update-world (world)
  "Perform a physics step in WORLD, updating its particles."
  (accumulate-forces world)
  (verlet-step world)
  (apply-constraints world))

(defun apply-force (world particle-id force)
  "Applies FORCE, a 2d vector, to particle with PARTICLE-ID. This force will
take effect in the next physics step of WORLD."
  (v+! (aref (forces world) particle-id) force))

(defun accumulate-forces (world)
  (when (gravity world)
    (loop for a across (forces world)
          do (decf (vy a) (gravity world)))))

(defun verlet-step (world)
  (loop for p across (particles world)
        for a across (forces world)
        for p-fixed? across (fixed world)
        when (not p-fixed?)
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
  (let* ((connection (v- (particle-position world p2-id)
                         (particle-position world p1-id))))
    ;; Edge case: particles are right on top of each other.
    ;; Give 'em a nudge.
    (when (and (zerop (vx connection))
               (zerop (vy connection)))
      (incf (vx connection) 1))
    (let ((diff (- (v-length connection) len)))
      ;; Each particle should move equally to make the spring closer to its
      ;; target length, so scale the diff by 0.5.
      ;; Include COEFF (0-1) in the scaling: the springier the spring, the less
      ;; the particles move towards their new position.
      (v-rescale! (* 0.5 coeff diff) connection)
      (try-translate-particle world p1-id connection)
      ;; Flip the diff so that it moves the other particle in the right direction.
      (v-scale! -1 connection)
      (try-translate-particle world p2-id connection))))

(defun apply-constraints (world)
  (loop repeat (verlet-iterations world)
        do (map nil
                (lambda (constraint)
                  (apply-constraint world constraint))
                (constraints world))))

(defun add-bounds (world max-x max-y &key (min-x 0) (min-y 0))
  "Add bounds to the physics world, particles are prevented from
going outside these bounds."
  (add-constraint world
                  (make-instance 'bounds-constraint
                                 :min-x min-x
                                 :max-x max-x
                                 :min-y min-y
                                 :max-y max-y)))

(defun add-stick-constraint (world p1-id p2-id &key len)
  "Adds a stick constraint between two particles. The particles will always
remain at a distance of exactly LEN from each other. If LEN is not provided, it
defaults to their current distance from each other."
  (add-constraint world
                  (make-instance 'stick-constraint
                                 :p1-id p1-id
                                 :p2-id p2-id
                                 :len (or len
                                          (euclidean-distance
                                           (particle-position world p1-id)
                                           (particle-position world p2-id))))))

(defun add-spring-constraint (world p1-id p2-id coeff &key len)
  "Adds a spring constraint between two particles. Like a stick constraint,
except 'springy'. COEFF must be between 0-1, this determines how quickly
the particles are forced back to being a distance of LEN apart. With COEFF=0, they
don't move at all. With COEFF=1, it's equivalent to a stick constraint."
  (assert (<= 0 coeff 1))
  (add-constraint world
                  (make-instance 'spring-constraint
                                 :p1-id p1-id
                                 :p2-id p2-id
                                 :coeff coeff
                                 :len (or len
                                          (euclidean-distance
                                           (particle-position world p1-id)
                                           (particle-position world p2-id))))))
