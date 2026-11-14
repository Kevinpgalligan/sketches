;;;; Marching squares!
;;
;; This is an implementation of the marching squares algorithm, heavily
;; based on this video by The Coding Train:
;;   https://www.youtube.com/watch?v=0ZONMNUKTfU
;;
;; Also helpful was this article referenced by that video:
;;   https://jamie-wong.com/2014/08/19/metaballs-and-marching-squares/
;;
;; TODO...
;;  0. Save some intermediate results (for reference).
;;  1. Refactor the defsketch so that fits on 1 screen.
;;  2. Linear interpolation!

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
  (noise-seed (get-universal-time))
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
             (v-avg (v1 v2)
               (v-scale! .5 (v+ v1 v2)))
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
                (dv (pt-noise d))
                (a-b (v-avg a b))
                (a-c (v-avg a c))
                (b-d (v-avg b d))
                (c-d (v-avg c d)))
            ;; Now, apply the binary threshold to corner values and handle all 16 possible
            ;; cases. 'a', 'b', 'c', and 'd' correspond to the top-left, top-right, bottom-left
            ;; and bottom-right corners. After thresholding, the outcomes can be indexed as
            ;; a 4-bit number and handled individually.
            (case (+ (to-bin av) (* #b10 (to-bin bv)) (* #b100 (to-bin cv)) (* #b1000 (to-bin dv)))
              ;; Skipping the zero case, it's a completely empty cell.
              (1
               ;; 'a' is "inside" the boundary, so fill in that corner of the cell.
               ;; For now, we don't do linear interpolation between corner values...
               (fc a a-b a-c))
              (2 (fc a-b b b-d))
              (3 (fc a b b-d a-c))
              (4 (fc c a-c c-d))
              (5 (fc a a-b c-d c))
              (6 (fc b b-d c-d c a-c a-b))
              (7 (fc a b b-d c-d c))
              (8 (fc d c-d b-d))
              (9 (fc a a-b b-d d c-d a-c))
              (10 (fc a-b b d c-d))
              (11 (fc a b d c-d a-c))
              (12 (fc c a-c b-d d))
              (13 (fc a a-b b-d d c))
              (14 (fc a-b b d c a-c))
              (15 (fc a b d c))))))))
  (stop-loop))
