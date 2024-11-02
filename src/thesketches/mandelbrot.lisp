;;; Coding Train: https://www.youtube.com/watch?v=6z7GQewK-Ks

;;; Mandelbrot set:
;;;    "Set of complex numbers c for which the function f(z) = z^2+c
;;;     doesn't diverge when iterated from z=0, i.e., for which the
;;;     sequence f(0), f(f(0)), ... remains bounded by an absolute value."
;;; (Wikipedia).

(sketches:def-sketch-package mandelbrot)
(in-package kg.sketch.mandelbrot)

(defsketch mandelbrot
    ((width 600)
     (height 600)
     (min-val -1.0)
     (max-val +1.0)
     (bound 16.0)
     (max-iters 100)
     (the-set (make-mandelbrot-image width height min-val max-val max-iters bound)))
  (background +black+)
  (draw the-set)
  (stop-loop))

(defun make-mandelbrot-image (width height min-val max-val max-iters bound)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (inline remap)
           (fixnum width height max-iters)
           (single-float min-val max-val bound))
  (let ((canvas (make-canvas width height)))
    (dotimes (x width)
      (dotimes (y height)
        (declare (fixnum x y))
        (let ((c (complex (remap x 0 width min-val max-val)
                          (remap y 0 height min-val max-val)))
              (z (complex 0.0 0.0))
              (n 0))
          (declare (fixnum n)
                   (type (complex single-float) z c))
          (loop while (and (< n max-iters) (< (abs z) bound))
                do (setf z (+ (* z z) c))
                do (incf n))
          (canvas-paint-gray255 canvas
                                (if (= n max-iters)
                                    0
                                    (let ((bright (remap n 0 max-iters 0 1)))
                                      (truncate (remap (sqrt bright) 0.0 1.0 0 255))))
                                x
                                y))))
    (canvas-lock canvas)
    canvas))
