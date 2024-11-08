(in-package sketches)

(defclass flowfield ()
  ((noise-state :initarg :noise-state)
   (spacing :initarg :spacing)
   (strength :initarg :strength)
   (noise-coords-scale :initarg :noise-coords-scale)
   (angle-range :initarg :angle-range)
   (side-length :initarg :side-length)
   (t0 :initarg :t0)))

(defun make-flowfield (&key (spacing 10) (strength 0.5)
                         (noise-coords-scale 0.7) (angle-range 8) (side-length 250)
                         (seed 7) (noise-state (noisy:make-noise 3 :seed seed)))
  (make-instance 'flowfield
                 :noise-state noise-state
                 :spacing spacing
                 :strength strength
                 :noise-coords-scale noise-coords-scale
                 :angle-range angle-range
                 :side-length side-length
                 :t0 0))

(defun flowfield-get-effect (flowfield position)
  "Finds the nearest point to POSITION (vec2) in a flow field and returns
the effect the flow field will have on that position."
  (with-slots (spacing strength noise-coords-scale t0 side-length)
      flowfield
    ;; While the grid points of the flowfield may lie on integer coordinates,
    ;; we want to scale it down because Perlin noise is always 0 at those points.
    (let* ((flow-pos (v-scale! (float (/ side-length))
                               (get-nearest-point-on-grid position spacing)))
           (angle (calc-flowfield-angle flowfield flow-pos)))
      (v-scale strength (get-pos-on-unit-circle angle)))))

(defun calc-flowfield-angle (flowfield flow-pos)
  (with-slots (noise-state t0 noise-coords-scale angle-range) flowfield
    (* angle-range pi (noisy:noise-gen noise-state
                             t0
                             (* noise-coords-scale (vx flow-pos))
                             (* noise-coords-scale (vy flow-pos))))))

(defun flowfield-inc-time! (flowfield dt)
  (incf (slot-value flowfield 't0) dt))

(defmethod draw ((instance flowfield) &key (colour +red+) width height &allow-other-keys)
  (with-slots (spacing side-length) instance
    (with-pen (make-pen :stroke colour)
      (loop for x = 0 then (+ x spacing)
            while (<= x width)
            do (loop for y = 0 then (+ y spacing)
                     while (<= y height)
                     do (progn
                          (circle x y (/ spacing 10))
                          (let* ((angle (calc-flowfield-angle instance
                                                              (v-scale! (/ side-length)
                                                                        (vec2 x y))))
                                 (end (v+
                                       (vec2 x y)
                                       (v-scale 10 (get-pos-on-unit-circle angle)))))
                            (line x y (vx end) (vy end)))))))))
