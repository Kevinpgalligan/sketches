(in-package sketches)

(defsketch noisecomp
    ((title "Comparison!")
     (N-perlin (make-perlin-noise 1 :seed 7))
     (N-value (make-vnoise :seed 5))
     (x-scale 0.1)
     (noise-scale 100)
     (dx 0.5)
     (offset 200))
  (let ((perlin-coords
          (loop for x = 0 then (+ x dx)
                while (< x width)
                collect x
                collect (* noise-scale (noise-get N-perlin (* x-scale x)))))
        (value-coords
          (loop for x = 0 then (+ x dx)
                while (< x width)
                collect x
                collect (+ offset (* noise-scale (noise-get N-value (* x-scale x)))))))
    (with-pen (make-pen :fill +blue+ :stroke +blue+)
      (apply #'polyline perlin-coords))
    (apply #'polyline value-coords)))

(defmethod setup ((instance noisecomp) &key &allow-other-keys)
  (background +white+))
