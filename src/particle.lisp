(in-package sketches)

(defclass particle ()
  ((pos :initarg :pos :accessor pos)
   (velocity :initarg :velocity :accessor velocity)
   (prev-position :initarg :prev-position :accessor prev-position)))

(defun make-particle (x y &key (vx 0) (vy 0))
  (let ((initial-position (vec2 x y)))
    (make-instance 'particle
                   :pos initial-position
                   :velocity (vec2 vx vy)
                   :prev-position initial-position)))

(defun update-particle-state! (particle dv max-speed min-x max-x min-y max-y &key wrap-around-p)
  "dv: change in velocity"
  (setf (prev-position particle) (pos particle))
  (setf (pos particle)
        (v+ (pos particle) (velocity particle)))
  (when (and wrap-around-p
             (or (outside-range-p min-x max-x (vx (pos particle)))
                 (outside-range-p min-y max-y (vy (pos particle)))))
    (setf (vx (pos particle)) (mod (vx (pos particle)) max-x)
          (vy (pos particle)) (mod (vy (pos particle)) max-y)
          ;; Don't want particle to appear as if it's just
          ;; teleported across the screen.
          (prev-position particle) (pos particle)))
  (setf (velocity particle)
        (v-clamp max-speed (v+ (velocity particle) dv))))
