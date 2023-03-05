(in-package sketches)

;; Ideas: maybe I need to store the displacement of each dot and
;; have a slight force that gradually brings each dot back to its
;; original position (like a spring, further away = more pull).

(defsketch dots
    ((width 500)
     (height 500)
     (side-length 300)
     (dot-spacing 50)
     (dot-diameter 10)
     (dot-max-displacement 40)
     (flowfield (make-flowfield (make-vnoise) :spacing 100 :strength 3))
     (rng (random-state:make-generator 'random-state:mersenne-twister-64))
     (particles (loop repeat 1
                      collect (make-particle (random-state:random-int rng 0 side-length)
                                             (random-state:random-int rng 0 side-length))))
     (draw-particles nil)
     (max-velocity 3)
     (push-effective-distance 120)
     (push-strength 150))
  (background +black+)
  (with-centered (width height side-length side-length)
    (loop for particle in particles
          do (update-particle-state! particle
                                     (flowfield-get-effect flowfield (pos particle))
                                     max-velocity
                                     0
                                     side-length
                                     0
                                     side-length)
          when draw-particles
            do (with-pen (make-pen :fill +red+)
                 (circle (vx (pos particle)) (vy (pos particle)) 3)))
    (loop for y = 0 then (+ y dot-spacing)
          while (<= y side-length)
          do (loop for x = 0 then (+ x dot-spacing)
                   while (<= x side-length)
                   do (let* ((pos (vec2 x y))
                             (displacement
                               (v-clamp dot-max-displacement
                                        (reduce #'v+
                                                (loop for particle in particles
                                                      collect (let* ((direction (shortest-connecting-line-in-box-with-wrap
                                                                                 (pos particle) pos side-length side-length))
                                                                     (dist (v-length direction))
                                                                     (total-strength (if (zerop dist)
                                                                                         0
                                                                                         (min push-strength (/ push-strength (sqrt dist))))))
                                                                (v-rescale total-strength direction)))
                                                :initial-value (vec2 0 0))))
                             (shifted-pos (v+ pos displacement)))
                        (with-pen (make-pen :fill +white+)
                          (circle (vx shifted-pos) (vy shifted-pos) (halve dot-diameter))))))))

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
