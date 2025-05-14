(sketches:def-sketch-package physics-test)
(in-package kg.sketch.physics-test)

(defsketch physics
    ((width 200)
     (height 200)
     (world (make-world))
     (pids nil)
     (y-axis :up))
  (loop for pid in pids
        do (with-particle-xy (x y)
                             world
                             pid
             (with-pen (:fill +white+)
               (circle x y 10))))
  (update-world world))

(defmethod setup ((instance physics) &key &allow-other-keys)
  (with-slots (world width height pids) instance
    (loop repeat 10
          do (push
              (add-particle world :x (random width) :y (random height))
              pids))
    (add-bounds world width height)
    (enable-gravity world 0.1)))
