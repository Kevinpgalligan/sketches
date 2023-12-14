(sketches:def-sketch-package squarez)
(in-package kg.sketch.squarez)

(defsketch squarez
    ((width 500)
     (height 500)
     (grid-width 7)
     (grid-offset (halve (* (/ 1 grid-width) width)))
     (ssize 25)
     (jitter 8))
  (background +black+)
  (dotimes (i grid-width)
    (dotimes (j grid-width)
      (let ((basex (+ grid-offset (* (/ i grid-width) width)))
            (basey (+ grid-offset (* (/ j grid-width) height))))
        (with-identity-matrix
          (translate basex basey)
          (rotate (* 90 (random 1.0)))
          (let ((coords
                  (alexandria:flatten
                   (loop for (x y) in (list (list (- (halve ssize)) (- (halve ssize)))
                                            (list (halve ssize) (- (halve ssize)))
                                            (list (halve ssize) (halve ssize))
                                            (list (- (halve ssize)) (halve ssize)))
                          collect (list
                                   (+ x (- jitter)
                                      (random (* 2 jitter)))
                                   (+ y (- jitter)
                                      (random (* 2 jitter))))))))
            (apply #'polygon coords))))))
  (stop-loop))
