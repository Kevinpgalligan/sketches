(sketches:def-sketch-package physics-test)
(in-package kg.sketch.physics-test)

;;;; A test of my Verlet physics implementation.
;;;; Particles, gravity, bounds, springs, sticks.

(defsketch physics
    ((width 200)
     (height 200)
     (world (make-world))
     (pal (random-palette))
     (pids nil)
     (spring-ids nil)
     (shape-ids nil)
     (y-axis :up))
  (reset-palette pal)
  (background (next-colour pal))
  (with-pen (:fill (next-colour pal))
    (loop for pid in pids
          do (with-particle-xy (x y)
                               world
                               pid
               (circle x y 5))))
  (with-pen (:stroke (next-colour pal))
    (apply #'line
           (loop for pid in spring-ids
                 for pos = (particle-position world pid)
                 collect (vx pos)
                 collect (vy pos))))
  (with-pen (:fill (next-colour pal))
    (apply #'polygon
           (loop for pid in shape-ids
                 for pos = (particle-position world pid)
                 collect (vx pos)
                 collect (vy pos))))
  (update-world world))

(defmethod setup ((instance physics) &key &allow-other-keys)
  (with-slots (world width height pids spring-ids shape-ids) instance
    (let ((p0 (add-particle world :x 100 :y 200 :fixed t))
          (p1 (add-particle world :x 50 :y 150))
          (p2 (add-particle world :x 100 :y 100))
          (p3 (add-particle world :x 50 :y 50))
          (p4 (add-particle world :x 10 :y 70)))
      (add-spring-constraint world p0 p1 0.01)
      (add-stick-constraint world p1 p2)
      (add-stick-constraint world p2 p3)
      (add-stick-constraint world p3 p4)
      (add-stick-constraint world p4 p1)
      (add-stick-constraint world p1 p3)
      (loop for id in (list p0 p1)
            do (push id spring-ids))
      (loop for id in (list p0 p1 p2 p3 p4)
            do (push id pids))
      (loop for id in (list p1 p2 p3 p4)
            do (push id shape-ids)))
    (add-bounds world width height)
    (enable-gravity world 0.02)))
