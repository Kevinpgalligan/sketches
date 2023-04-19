(in-package sketches)

(defsketch groove2
    ((width 400)
     (height 600)
     (rng (random-state:make-generator 'random-state:mersenne-twister-64 7))
     (dx-bigdot 28)
     (dx-tinydot 11)
     (offset-filled-dot-x 7)
     (offset-filled-dot-y 5)
     (hollowdot-coords nil)
     (filleddot-coords nil)
     (tinydot-coords nil)
     (max-shift 15))
  (background +white+)
  (loop for (x y) in hollowdot-coords
        do (circle x y 4))
  (with-pen (make-pen :fill +black+)
    (loop for (x y) in filleddot-coords
          do (circle x y 3))
    (loop for (x y) in tinydot-coords
          do (circle x y 1))))

(defmethod setup ((instance groove2) &key &allow-other-keys)
  (with-slots (width
               height
               dx-bigdot
               dx-tinydot
               offset-filled-dot-x
               offset-filled-dot-y
               hollowdot-coords
               filleddot-coords
               tinydot-coords
               rng
               max-shift)
      instance
    (loop for x = 0 then (+ x dx-bigdot)
          while (< x width)
          do (loop for y = 0 then (+ y dx-bigdot)
                   while (< y height)
                   do (push (shifted-point x y rng max-shift) hollowdot-coords)
                   do (push (shifted-point (+ offset-filled-dot-x x)
                                           (+ offset-filled-dot-y y)
                                           rng
                                           max-shift)
                            filleddot-coords)))
    (loop for x = 0 then (+ x dx-tinydot)
          while (< x width)
          do (loop for y = 0 then (+ y dx-tinydot)
                   while (< y height)
                   do (push (shifted-point x y rng max-shift) tinydot-coords)))))

(defun shifted-point (x y rng max-shift)
  (list (+ x (random-state:random-float rng 0 max-shift))
        (+ y (random-state:random-float rng 0 max-shift))))
