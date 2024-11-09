;;;; Attempting to recreate Unknown Pleasures album cover.

(sketches:def-sketch-package unknown)
(in-package kg.sketch.unknown)

(defsketch unknown
    ((width 400)
     (height 600)
     (x-offset 40)
     (y-offset 60)
     (x-noise-scale 0.01)
     (y-noise-scale 0.9)
     (dx 5)
     (dy 10)
     (z 0) ; time
     (dz 0.01)
     (max-height (* 7 dy))
     (num-lines 40)
     (decay 0.9))
  (background +black+)
  (loop for i = 0 then (1+ i)
        for y = (+ y-offset (* i dy))
        while (< y (- height y-offset))
        do (let ((colour +white+))
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
                                                 (noise (* x-noise-scale x) (* y-noise-scale y) z)
                                                 (smoothstep (- 1 (/ offset-from-mid (/ (- width (* 2 y-offset)) 2))))))))))))
  (incf z dz))

(defmethod setup ((instance unknown) &key &allow-other-keys)
  (noise-seed (random 100))
  (noise-detail :lod 1))
