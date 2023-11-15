(sketches:def-sketch-package heightmap)
(in-package kg.sketch.heightmap)

(defsketch heightmap
    ((width 600)
     (height 400)
     (tile-width 10)
     ;; Value noise versus perlin noise.
     ;(N (make-vnoise))
     (N (make-perlin-noise 3 :seed 7))
     (noise-coords-scale 0.02)
     ;; Time dimension.
     (z 0)
     (dz 0.05))
  (incf z dz)
  (dotimes (i (/ height tile-width))
    (dotimes (j (/ width tile-width))
      (let* ((x (* j tile-width))
             (y (* i tile-width))
             (colour (hsb 0 0 (noise-get N (* noise-coords-scale x) (* noise-coords-scale y) z))))
        (with-pen (make-pen :weight 1 :stroke colour :fill colour)
          (rect x y tile-width tile-width))))))
