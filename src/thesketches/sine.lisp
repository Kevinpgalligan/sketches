(sketches:def-sketch-package sine)
(in-package kg.sketch.sine)

;;;; Trying to make noise out of sine waves.

(defun make-sine (freq amp phase)
  (lambda (x)
    (* amp (sin (+ phase (* freq x))))))

(defun make-sum (&rest fs)
  (lambda (&rest args)
    (loop for f in fs
          sum (apply f args))))

(defsketch sine
    ((width 500)
     (height 500)
     (n 100)
     (t0 0)
     (steps 250)
     (f (apply #'make-sum
               (loop repeat n
                     collect (make-sine (+ 1/100 (random .2))
                                        (+ 2 (random 5))
                                        (random 50))))))
  (background +black+)
  (with-pen (make-pen :stroke +white+ :weight 3)
    (apply #'polyline
           (loop with step-size = (/ width steps)
                 for i upto steps
                 for x = (* i step-size)
                 collect x
                 collect (+ (/ height 2) (funcall f (+ t0 x))))))
  (incf t0 1))
