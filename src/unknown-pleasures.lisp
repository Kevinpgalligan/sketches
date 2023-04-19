;;;; Attempting to recreate Unknown Pleasures album cover.

(in-package sketches)

(defsketch unknown
    ((width 400)
     (height 600)
     (x-offset 40)
     (y-offset 40)
     (N (make-perlin-noise 3))
     (N-colour (make-vnoise))
     (x-noise-scale 0.05)
     (y-noise-scale 5)
     (dx 5)
     (dy 10)
     (z 0) ; time
     (dz 0.01)
     (max-height (* 5 dy))
     (num-lines 40)
     (decay 0.9))
  (background +black+)
  (loop for i = 0 then (1+ i)
        for y = (+ y-offset (* i dy))
        while (< y (- height y-offset))
        do (let ((colour (hsb (noise-get N-colour (* y-noise-scale y)) 0.8 0.8)))
             (with-pen (make-pen :fill colour :stroke colour :weight 1.5)
               (apply #'polyline
                      (loop for j = 0 then (1+ j)
                            for x = (+ x-offset (* j dx))
                            for offset-from-mid = (abs (- (/ width 2) x))
                            while (< x (- width x-offset))
                            collect x
                            ;; Reduce effect of noise as we go out from
                            ;; the center.
                            collect (min y
                                         (- y (* max-height
                                                 (noise-get N (* x-noise-scale x) (* y-noise-scale y) z)
                                                 (smoothstep (- 1 (/ offset-from-mid (/ (- width (* 2 y-offset)) 2))))))))))))
  (incf z dz))
