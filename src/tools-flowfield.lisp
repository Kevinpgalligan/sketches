(in-package sketches)

(defclass flowfield ()
  ((spacing :initarg :spacing)
   (strength :initarg :strength)
   (N :initarg :N)
   (t0 :initarg :t0)))

(defun make-flowfield (N &key (spacing 10) (strength 0.5))
  (make-instance 'flowfield
                 :spacing spacing
                 :strength strength
                 :N N
                 :t0 0))

(defun flowfield-get-effect (flowfield position)
  "Finds the nearest point to POSITION (vec2) in a flow field and returns
the effect the flow field will have on that position."
  (with-slots (spacing strength N t0)
      flowfield
    (let* ((flow-pos (get-nearest-point-on-grid position spacing))
           (angle (* 2 pi (apply #'noise-get (concatenate 'list (list N t0) flow-pos)))))
      (v-scale strength (get-pos-on-unit-circle angle)))))

(defun flowfield-inc-time! (flowfield dt)
  (incf (slot-value flowfield 't0) dt))
