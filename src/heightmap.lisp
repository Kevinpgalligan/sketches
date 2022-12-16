(in-package sketches)

(defsketch heightmap
    ((width 600)
     (height 400)
     (tile-width 10)
     (N (make-vnoise :dimensions 3))
     (noise-coords-scale 0.02)
     ;; Time dimension.
     (z 0)
     (dz 0.01))
  (incf z dz)
  (dotimes (i (/ height tile-width))
    (dotimes (j (/ width tile-width))
      (let* ((x (* j tile-width))
             (y (* i tile-width))
             (colour (hsb 0 0 (noise-get N (* noise-coords-scale x) (* noise-coords-scale y) z))))
        (with-pen (make-pen :weight 1 :stroke colour :fill colour)
          (rect x y tile-width tile-width))))))
