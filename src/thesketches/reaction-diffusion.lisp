;;;; Made with Tristan while at the Recurse Center.
;;;; Based on this Coding Train video:
;;;;   https://www.youtube.com/watch?v=BV9ny785UNc

(sketches:def-sketch-package reaction-diffusion)
(in-package kg.sketch.reaction-diffusion)

(defun make-grid (grid-size)
  (let ((spot-size (/ grid-size 6))
        (mid (/ grid-size 2)))
    (make-array
     (list grid-size grid-size 2)
     :element-type 'single-float
     :initial-contents
     (loop for i from 0 below grid-size
           collect (loop for j from 0 below grid-size
                         collect (if (and (> i (- mid (/ spot-size 2)))
                                          (< i (+ mid (/ spot-size 2)))
                                          (> j (- mid (/ spot-size 2)))
                                          (< j (+ mid (/ spot-size 2))))
                                     (list 1.0 1.0)
                                     (list 1.0 0.0)))))))

(defsketch react
    ((width 400)
     (height 400)
     (grid-size 50)
     (grid (make-grid grid-size))
     (old-grid (make-grid grid-size))
     (canvas (make-canvas grid-size grid-size))
     (dt 1.0)
     (da 1.0)
     (db 0.5)
     (feed 0.0545)
     (k 0.062))
  (background +black+)
  (rotatef old-grid grid)
  (update-state old-grid grid grid-size da db feed k dt)
  (draw-grid grid grid-size canvas width height))

(defun update-state (old-grid grid grid-size da db feed k dt)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array single-float (* * 2)) old-grid grid)
           (fixnum grid-size)
           (single-float da db feed k dt))
  (loop for i from 0 below grid-size
        do (loop for j from 0 below grid-size
                 do (progn
                      (setf (aref grid i j 0)
                            (calculate-a i j old-grid grid-size da feed dt)
                            (aref grid i j 1)
                            (calculate-b i j old-grid grid-size db feed k dt))))))

(defun calculate-a (i j old-grid grid-size da feed dt)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (fixnum i j grid-size)
           (single-float feed dt da)
           (type (simple-array single-float (* * 2)) old-grid))
  (let ((a (aref old-grid i j 0))
        (b (aref old-grid i j 1)))
    (alexandria:clamp
     (+ a
        (* dt
           (+ (* da (laplace old-grid i j grid-size 0))
              (- (* a b b))
              (* feed (- 1 a)))))
     0.0 1.0)))

(defun calculate-b (i j old-grid grid-size db feed k dt)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (fixnum i j grid-size)
           (single-float db feed k dt)
           (type (simple-array single-float (* * 2)) old-grid))
  (let ((a (aref old-grid i j 0))
        (b (aref old-grid i j 1)))
    (alexandria:clamp
     (+ b
        (* dt
           (+ (* db (laplace old-grid i j grid-size 1))
              (* a b b)
              (- (* (+ k feed) b)))))
     0.0 1.0)))

(declaim (ftype (function ((simple-array single-float (* * 2))
                           fixnum
                           fixnum
                           fixnum
                           fixnum)
                          single-float)
                laplace))
(defun laplace (old-grid i j grid-size get-index)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (fixnum i j grid-size get-index)
           (type (simple-array single-float (* * 2)) old-grid))
  (let ((conv '((0.05 0.2 0.05)
                (0.2 -1.0 0.2)
                (0.05 0.2 0.05))))
    (loop for row in conv
          for i-offset fixnum = -1 then (1+ i-offset)
          sum (loop for mul single-float in row
                    for j-offset fixnum = -1 then (1+ j-offset)
                    sum (* mul (aref old-grid
                                     (mod (+ i i-offset) grid-size)
                                     (mod (+ j j-offset) grid-size)
                                     get-index)) single-float) single-float)))

(defun draw-grid (grid grid-size canvas width height)
  (canvas-unlock canvas)
  (loop for i from 0 below grid-size
        do (loop for j below grid-size
                 do (let* ((a (aref grid i j 0))
                           (b (aref grid i j 1)))
                      (canvas-paint-gray255 canvas
                                            (truncate
                                             (remap (alexandria:clamp (- a b) 0 1)
                                                    0 1
                                                    0 255))
                                            j
                                            i))))
  (canvas-lock canvas :mag-filer :nearest :min-filter :nearest)
  (draw canvas :width width :height height :mag-filter :nearest :min-filter :nearest))
