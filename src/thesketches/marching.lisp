;;;; Marching squares!
;;
;; This is an implementation of the marching squares algorithm, heavily
;; based on this video by The Coding Train:
;;   https://www.youtube.com/watch?v=0ZONMNUKTfU
;;
;; Also helpful was this article, referenced in that video:
;;   https://jamie-wong.com/2014/08/19/metaballs-and-marching-squares/

(sketches:def-sketch-package marching)
(in-package kg.sketch.marching)

(defsketch marching-squares
    ((width 500)
     (height 500)
     (res 20)
     (cellwidth (floor width res))
     (noise-scale 0.01)
     (threshold 0.5)
     (pen (make-pen :fill +black+ :stroke nil)))
  (background +white+)
  (noise-seed 26)
  ;; Iterate over all cells.
  (dotimes (i cellwidth)
    (dotimes (j cellwidth)
      (flet ((pt-noise (pt)
               (noise (* noise-scale (vx pt))
                      (* noise-scale (vy pt))))
             (to-bin (v)
               (if (>= v threshold)
                   1
                   0))
             (interp (p1 v1 p2 v2)
               ;; p1 is "inside", i.e. v1 is above the threshold. While v2 is below.
               ;; Interpolate between them based on the threshold value.
               (let ((d (/ (- threshold v2)
                           (- v1 v2))))
                 (v+! (v-scale! d (v- p1 p2)) p2)))
             (fc (&rest pts) ; Fill Cell
               (with-pen pen
                 (apply #'polygon
                        (loop for pt in pts
                              collect (vx pt)
                              collect (vy pt))))))
        ;; Calculate the corner positions and corner values of each cell.
        ;; We could do a rolling window sorta thing to avoid having
        ;; to recalculate shared corners. But maybe this is already fast enough.
        (let ((a (vec2 (* i res) (* j res)))
              (b (vec2 (* (1+ i) res) (* j res)))
              (c (vec2 (* i res) (* (1+ j) res)))
              (d (vec2 (* (1+ i) res) (* (1+ j) res))))
          (let ((av (pt-noise a))
                (bv (pt-noise b))
                (cv (pt-noise c))
                (dv (pt-noise d)))
            ;; Now, apply the binary threshold to corner values and handle all 16 possible
            ;; cases. 'a', 'b', 'c', and 'd' correspond to the top-left, top-right, bottom-left
            ;; and bottom-right corners. After thresholding, the combined values of the corners can
            ;; be represented as a 4-bit number and handled individually. 
            (case (+ (to-bin av) (* #b10 (to-bin bv)) (* #b100 (to-bin cv)) (* #b1000 (to-bin dv)))
              ;; Skipping the zero case, it's a completely empty cell.
              (1
               ;; 'a' is "inside" the boundary (above the threshold), so fill in that corner of the cell.
               ;; For now, we don't do linear interpolation between corner values...
               (fc a (interp a av b bv) (interp a av c cv)))
              (2 (fc (interp b bv a av) b (interp b bv d dv)))
              (3 (fc a b (interp b bv d dv) (interp a av c cv)))
              (4 (fc c (interp c cv a av) (interp c cv d dv)))
              (5 (fc a (interp a av b bv) (interp c cv d dv) c))
              (6 (fc b (interp b bv d dv) (interp c cv d dv) c (interp c cv a av) (interp b bv a av)))
              (7 (fc a b (interp b bv d dv) (interp c cv d dv) c))
              (8 (fc d (interp d dv c cv) (interp d dv b bv)))
              (9 (fc a (interp a av b bv) (interp d dv b bv) d (interp d dv c cv) (interp a av c cv)))
              (10 (fc (interp b bv a av) b d (interp d dv c cv)))
              (11 (fc a b d (interp d dv c cv) (interp a av c cv)))
              (12 (fc c (interp c cv a av) (interp d dv b bv) d))
              (13 (fc a (interp a av b bv) (interp d dv b bv) d c))
              (14 (fc (interp b bv a av) b d c (interp c cv a av)))
              (15 (fc a b d c))))))))
  (stop-loop))
