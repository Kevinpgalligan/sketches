;;; Implementation of The Coding Train's first coding challenge, to
;;; show stars moving as we travel through space. I tweaked the math
;;; a bit 'cause I couldn't understand how his projection was working.
;;; As a result, I think mine looks a bit worse.

(in-package sketches)

(defstruct point x y z)

(defun random-int (low high)
  ;; Exclusive bounds.
  (+ 1 low (random (- high low 1))))

(defun init-point-randomly (p width height depth)
  (setf (point-x p) (random-int (/ (- width) 2) (/ width 2))
        (point-y p) (random-int (/ (- height) 2) (/ height 2))
        (point-z p) (random-int 0 depth)))

(defun project-star (s z depth star-radius)
  (let* ((scale (/ (- depth z) depth))
         (px (* scale (point-x s)))
         (py (* scale (point-y s)))
         (pr (* scale star-radius)))
    (values px py pr)))

(defsketch stars
    ((width 600)
     (height 400)
     (depth 300)
     (dz 2)
     (streak-steps 20)
     (streak-weight 0.2)
     (star-radius 1)
     (stars (loop repeat 100
                  collect (let ((poi (make-point)))
                            (init-point-randomly poi width height depth)
                            poi))))
  (background +black+)
  ;; The stars are based around (0,0), so shift their coordinates into
  ;; the canvas range.
  (translate (/ width 2) (/ height 2))
  (with-pen (make-pen :stroke +white+ :fill +white+)
    (loop for s in stars
          do (multiple-value-bind (px py pr) (project-star s (point-z s) depth star-radius)
               (circle px py pr)
               (with-pen (make-pen :weight streak-weight :stroke +white+ :fill +white+)
                 (multiple-value-bind (ox oy dud) (project-star s (+ (point-z s) (* streak-steps dz)) depth star-radius)
                   (line ox oy px py))))
          do (decf (point-z s) dz)
          do (when (< (point-z s) 0)
               (init-point-randomly s width height depth)))))
