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

(defun draw-curve (f x-start x-end y-offset steps)
  (apply #'polyline
         (loop with step-size = (/ (- x-end x-start) steps)
               for i upto steps
               for x = (+ x-start (* i step-size))
               collect x
               collect (+ y-offset (funcall f x)))))

(defsketch sine
    ((width 500)
     (height 500)
     (n 5)
     (f (apply #'make-sum
               (loop repeat n
                     collect (make-sine (+ 50 (random 50))
                                        (+ 10 (random 50))
                                        (random 50))))))
  (draw-curve f 0 width (/ height 2) 500)
  (stop-loop))
