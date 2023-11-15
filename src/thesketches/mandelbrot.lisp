;;; Coding Train: https://www.youtube.com/watch?v=6z7GQewK-Ks

;;; Mandelbrot set:
;;;    "Set of complex numbers c for which the function f(z) = z^2+c
;;;     doesn't diverge when iterated from z=0, i.e., for which the
;;;     sequence f(0), f(f(0)), ... remains bounded by an absolute value."
;;; (Wikipedia).

(sketches:def-sketch-package mandelbrot)
(in-package kg.sketch.mandelbrot)

(defsketch mandelbrot
    ((width 100)
     (height 100)
     (min-val -1)
     (max-val +1)
     (bound 16)
     (max-iters 10)
     (the-set (make-mandelbrot-image width height min-val max-val max-iters bound)))
  (background +black+)
  (with-pen (make-pen :fill (canvas-image the-set))
    (rect 0 0 width height)))

(defun make-mandelbrot-image (width height min-val max-val max-iters bound)
  (let ((canvas (make-canvas width height)))
    (dotimes (x width)
      (dotimes (y height)
        (let ((c (complex (remap x 0 width min-val max-val)
                          (remap y 0 height min-val max-val)))
              (z 0)
              (n 0))
          (loop while (and (< n max-iters) (< (abs z) bound))
                do (setf z (+ (* z z) c))
                do (incf n))
          (canvas-paint canvas
                        (gray-255 (remap n 0 max-iters 0 255))
                        x
                        y))))
    (canvas-lock canvas)
    canvas))
