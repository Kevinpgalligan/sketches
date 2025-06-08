(sketches:def-sketch-package keffiyeh)
(in-package kg.sketch.keffiyeh)

(defsketch keffiyeh
    ((height 600))
  (background +white+)
  (let ((black-pen (make-pen :fill +black+ :stroke +black+))
        (types '(:line :waves :mottled :leaves :grid :curly-bracks))
        (special-types '(:leaves :grid :curly-bracks))
        last-type)
    (with-pen black-pen
      (loop with y = 0
            while (< y height)
            do (let ((type (alexandria:random-elt
                            (remove-if (lambda (type)
                                         (or (eq type last-type)
                                             ;; These look too similar.
                                             (and (member type special-types)
                                                  (member last-type special-types))))
                                       types))))
                 (setf last-type type)
                 (incf y
                       (+ 12
                          (case type
                            (:line (keffiyeh-line y width))
                            (:waves (keffiyeh-waves y width black-pen))
                            (:mottled (keffiyeh-mottled y width))
                            (:leaves (keffiyeh-leaves y width))
                            (:grid (keffiyeh-grid y width black-pen))
                            (:curly-bracks (keffiyeh-curly-bracks y width))
                            (t (error "Unknown type")))))))))
  (stop-loop))

(defun keffiyeh-line (y width)
  (let ((weight (+ 3 (random 4))))
    (rect 0 (+ y 5) width weight)
    weight))

(defun keffiyeh-waves (y width black-pen)
  (let ((amp (+ 2 (random 3)))
        (pen (copy-pen black-pen))
        (weight 2))
    (setf (pen-weight pen) weight)
    (setf (pen-line-join pen) :none)
    (with-pen pen
      (flet ((drawsine (offset)
               (apply #'polyline
                      (loop for x = 0 then (+ x 0.3)
                            while (< x width)
                            collect x
                            collect (+ offset
                                       (* amp (sin (* x 0.1))))))))
        (drawsine (+ y amp))
        (drawsine (+ y amp (* 2 weight)))))
    (+ amp (* 2 weight))))

(defun keffiyeh-mottled (y width)
  (let* ((h (+ 5 (random 10)))
         (cvs (make-canvas width h)))
    (dotimes (i width)
      (dotimes (j h)
        (when (zerop (mod (+ i j) 2))
          (canvas-paint cvs +black+ i j))))
    (draw cvs :y y)
    h))

(defun keffiyeh-leaves (y width)
  (let ((rx 3)
        (ry 10))
    (loop for x = ry then (+ x (* 4 ry))
          while (< x width)
          do (with-translate (x (+ y ry))
               (with-rotate (-45)
                 (ellipse 0 0 rx ry))
               (with-translate ((* 1.6 ry) 0)
                 (with-rotate (45)
                   (ellipse 0 0 rx ry)))))
    (* 1.3 ry)))

(defun keffiyeh-grid (base-y width black-pen)
  (let ((rx 3)
        (ry 6)
        (layers (+ 3 (random 3)))
        (x-offset 20)
        (y-offset 16)
        (pen (copy-pen black-pen)))
    (setf (pen-weight pen) 2)
    (with-pen pen
      (loop for i from 0 below layers
            do (loop for j from 0
                     for x = (+ (* j x-offset)
                                (if (evenp i) 0 (floor x-offset 2)))
                     for y = (+ base-y (* i y-offset) ry)
                     while (< x (+ width x-offset))
                     do (with-translate (x y)
                          (with-rotate ((* 45 (expt -1 (+ i j))))
                            (ellipse 0 0 rx ry))
                          (when (not (= i (1- layers)))
                            (line 0 0 (halve x-offset) y-offset)
                            (line 0 0 (- (halve x-offset)) y-offset))))))
    (+ ry (* (1- layers) y-offset))))

(defun keffiyeh-curly-bracks (base-y width)
  (let* ((shape-scale 1)
         (shape (mapcar (lambda (c) (* c shape-scale))
                        ;; Manually inputted coordinates.
                        ;; Starting from origin at lower left corner.
                        '(0 0 3 -2 4 -2
                          5 -5 16 -5.5 18 -6
                          23 -10 24 -14 26 -16
                          28 -17 29 -18 30 -21
                          29 -23 25 -22 25 -19
                          19 -18 16 -14 15 -13
                          13 -14 11 -14 10 -12
                          4 -11 3 -10 2 -6
                          1 -4)))
         (shape-width
           (loop for x in shape by #'cddr
                 maximizing x into max-x
                 minimizing x into min-x
                 finally (return (- max-x min-x))))
         (shape-height
           (loop for y in (cdr shape) by #'cddr
                 maximizing y into max-y
                 minimizing y into min-y
                 finally (return (- max-y min-y)))))
    (loop for x = 0 then (+ x (* 2 shape-width))
          while (< (- x shape-width) width)
          do (with-translate (x (+ base-y shape-height))
               (apply #'polygon shape)
               (with-scale (-1 1)
                 (apply #'polygon shape))))
    shape-height))
