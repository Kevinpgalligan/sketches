(in-package sketches)

(defclass flowfield ()
  ((spacing :initarg :spacing)
   (strength :initarg :strength)
   (N :initarg :N)
   (noise-coords-scale :initarg :noise-coords-scale)
   (t0 :initarg :t0)))

(defun make-flowfield (noisegen &key (spacing 10) (strength 0.5) (noise-coords-scale 1))
  (make-instance 'flowfield
                 :spacing spacing
                 :strength strength
                 :N noisegen
                 :noise-coords-scale noise-coords-scale
                 :t0 0))

(defun flowfield-get-effect (flowfield position)
  "Finds the nearest point to POSITION (vec2) in a flow field and returns
the effect the flow field will have on that position."
  (with-slots (spacing strength N noise-coords-scale t0)
      flowfield
    (let* ((flow-pos (get-nearest-point-on-grid position spacing))
           (angle (* 2 pi (noise-get N
                                     t0
                                     (* noise-coords-scale (vx flow-pos))
                                     (* noise-coords-scale (vy flow-pos))))))
      (v-scale strength (get-pos-on-unit-circle angle)))))

(defun flowfield-inc-time! (flowfield dt)
  (incf (slot-value flowfield 't0) dt))
