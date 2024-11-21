(sketches:def-sketch-package nested-squares)
(in-package kg.sketch.nested-squares)

(defsketch nested-squares
    ((palette (get-palette :sadiq))
     (width 400)
     (height 400)
     (offset 10)
     (side 50))
  (dotimes (i (truncate width side))
    (dotimes (j (truncate height side))
      (let ((x0 (* j side))
            (y0 (* i side)))
        (with-pen (make-pen :fill (next-colour palette))
          (rect x0 y0 side side))
        (let* ((x1 (+ x0 offset))
               (y1 (+ y0 offset))
               (side1 (- side (* 2 offset)))
               (coords
                 (mapcar
                  (lambda (c)
                    ;; Some jitter for the inner square.
                    (+ c (random offset)))
                  (list x1 y1
                        (+ x1 side1) y1
                        (+ x1 side1) (+ y1 side1)
                        x1 (+ y1 side1)))))
          (with-pen (make-pen :fill (next-colour palette))
            (apply #'polygon coords)))))
    (shuffle-palette palette))
  (stop-loop))
