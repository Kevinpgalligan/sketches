(sketches:def-sketch-package dots)
(in-package kg.sketch.dots)

(defsketch dots
    ((width 500)
     (height 500)
     (side-length 300)
     (dot-spacing 20)
     (dot-diameter 10)
     (dot-max-displacement 40)
     (flowfield (make-flowfield :spacing 30 :strength 0.05))
     (rng (random-state:make-generator 'random-state:mersenne-twister-64))
     (particles (loop repeat 3
                      collect (make-particle (random-state:random-int rng 0 width)
                                             (random-state:random-int rng 0 height))))
     (draw-particles nil)
     (max-velocity 5)
     (push-strength 75))
  (background +black+)
  (loop for particle in particles
        do (update-particle-state! particle
                                   (flowfield-get-effect flowfield (pos particle))
                                   max-velocity
                                   0
                                   width
                                   0
                                   height)
        when (or (outside-range-p 0 width (vx (pos particle)))
                 (outside-range-p 0 height (vy (pos particle))))
          do (let ((to-centre (v- (vec2 (halve width) (halve height))
                                  (pos particle))))
               ;; Give it a boost back to the centre.
               (setf (velocity particle) (v-rescale max-velocity to-centre)))
        when draw-particles
          do (with-pen (make-pen :fill +red+)
               (circle (vx (pos particle)) (vy (pos particle)) 3)))
  (let ((x-offset (halve (- width side-length)))
        (y-offset (halve (- height side-length))))
    (loop for y = y-offset then (+ y dot-spacing)
          while (<= y (+ y-offset side-length))
          do (loop for x = x-offset then (+ x dot-spacing)
                   while (<= x (+ x-offset side-length))
                   do (let* ((pos (vec2 x y))
                             (displacement
                               (v-clamp dot-max-displacement
                                        (reduce #'v+
                                                (loop for particle in particles
                                                      collect (let* ((direction (v- pos (pos particle)))
                                                                     (dist (v-length direction))
                                                                     (total-strength (if (zerop dist)
                                                                                         0
                                                                                         (min push-strength (/ push-strength (sqrt dist))))))
                                                                (v-rescale total-strength direction)))
                                                :initial-value (vec2 0 0))))
                             (shifted-pos (v+ pos displacement)))
                        (with-pen (make-pen :fill +white+)
                          (circle (vx shifted-pos) (vy shifted-pos) (halve dot-diameter)))))))
  ;; Show the flowfield for debugging.
  ;(draw flowfield :width width :height height)
  )

(defun shortest-connecting-line-in-box-with-wrap (v1 v2 width height)
  "Direction vector from V1 to V2 in a rectangle where going out 1 edge will
bring you back out on the other side."
  (let ((x-offset (if (< (vx v2) (halve width))
                      width
                      (- width)))
        (y-offset (if (< (vy v2) (halve height))
                      height
                      (- height))))
    (alexandria:extremum
     (loop for (xoff yoff) in (list (list 0 0)
                                    (list 0 y-offset)
                                    (list x-offset 0)
                                    (list x-offset y-offset))
           collect (v- (vec2 (+ xoff (vx v2)) (+ yoff (vy v2))) v1))
     #'<
     :key #'v-length)))
