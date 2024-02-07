(sketches:def-sketch-package swirl)
(in-package kg.sketch.swirl)

(defclass sparticle (particle)
  ((colour :initarg :colour :reader colour)))

(defun random-particle (colour cells)
  (let ((start-pos (vec2 (random (float cells))
                         (random (float cells)))))
    (make-instance 'sparticle
                   :pos start-pos
                   :velocity (vec2 0 0)
                   :prev-position start-pos
                   :colour colour)))

(defun random-particles (n colour cells)
  (loop repeat n
        collect (random-particle colour cells)))

(defsketch swirl
    ((width 400)
     (height width)
     (sidelen 2)
     (cells (/ width sidelen))
     (copy-pixels t)
     (pal (get-palette :sadiq))
     (particles nil)
     (max-velocity 0.5)
     (force 0.25)
     (jitter-rads 1.2))
  (update-state particles max-velocity cells force jitter-rads)
  (paint-crossed-cells sidelen particles))

(defun update-state (particles max-velocity cells force jitter-rads)
  (loop for p in particles
        do (update-particle-state! p
                                   (calc-dv p cells force jitter-rads)
                                   max-velocity
                                   0 cells 0 cells
                                   :wrap-around-p nil)))

(defun calc-dv (p cells force jitter-rads)
  (let* ((cx (/ cells 2))
         (center (vec2 cx cx))
         (dir (v- (pos p) center)))
    (v-rescale! force
                (v-rotate (- (random jitter-rads))
                          (perpendicular-clockwise dir)))))

(defun paint-crossed-cells (sidelen particles)
  (loop for p in particles
        do (let ((x1 (floor (vx (prev-position p))))
                 (y1 (floor (vy (prev-position p))))
                 (x2 (floor (vx (pos p))))
                 (y2 (floor (vy (pos p)))))
             (loop for x from (min x1 x2) upto (max x1 x2)
                   do (loop for y from (min y1 y2) upto (max y1 y2)
                            do (with-pen (make-pen :fill (colour p))
                                 (rect (* sidelen x) (* sidelen y)
                                       sidelen sidelen)))))))

(defmethod setup ((instance swirl) &key &allow-other-keys)
  (with-slots (particles pal cells)
      instance
    (shuffle-palette pal)
    (background (next-colour pal))
    (setf particles
          (append
           (random-particles 80 (next-colour pal) cells)
           (random-particles 80 (next-colour pal) cells)
           (random-particles 80 (next-colour pal) cells)))))

(defmethod on-key ((instance swirl) (key (eql :space)) (state (eql :up)))
  (setup instance))
