(sketches:def-sketch-package reuleaux)
(in-package kg.sketch.reuleaux)

;;;; Construction of a reuleaux triangle, which is a curve of constant
;;;; width, based on Martin Gardner's Mathematical Games column.

(defun make-arc (c start end &key (steps 20) anticlockwise)
  (let* ((dir1 (v->polar! (v- start c)))
         (dir2 (v->polar! (v- end c)))
         (r (vr dir1)))
    (destructuring-bind (th1 th2)
        (cond
          ((and anticlockwise (> (v-theta dir1) (v-theta dir2)))
           (list (- (v-theta dir1) (* 2 pi)) (v-theta dir2)))
          ((and (not anticlockwise) (< (v-theta dir1) (v-theta dir2)))
           (list (v-theta dir1) (- (v-theta dir2) (* 2 pi))))
          (t (list (v-theta dir1) (v-theta dir2))))
      (loop for i from 0 upto steps
            for theta = (+ th1 (* (/ i steps) (- th2 th1)))
            collect (+ (vx c) (* r (cos theta)))
            collect (+ (vy c) (* r (sin theta)))))))

(defun draw-arc (c p1 p2 &key (steps 20))
  (apply #'polyline (make-arc c p1 p2 :steps steps)))

(defsketch reuleaux
    ((width 400)
     (height 400)
     (side 200)
     (gap (/ side 2))
     (t0 0)
     (dt 0.01)
     (y-axis :up))
  (background +white+)
  (let* ((c (/ width 2))
         (m (/ gap (cos (radians 30))))
         (offset (vec2 0 m))
         (p1 (v-rotate t0 (vec2 0 m)))
         (p2 (v-rotate (+ t0 (* 2 pi 1/3)) offset))
         (p3 (v-rotate (+ t0 (* 2 pi 2/3)) offset)))
    (rect gap gap side side)
    (with-current-matrix
      (translate c c)
      (let ((poly (append (make-arc p1 p2 p3)
                          (make-arc p2 p3 p1)
                          (make-arc p3 p1 p2))))
        (with-pen (make-pen :fill +red+ :stroke +black+)
          (apply #'polygon
                 (loop with x-offset = (loop for (x y) on poly by #'cddr
                                             maximizing (- (- gap) x))
                       with y-offset = (loop for (x y) on poly by #'cddr
                                             maximizing (- (- gap) y))
                       for (x y) on poly by #'cddr
                       collect (+ x x-offset)
                       collect (+ y y-offset)))))))
  (incf t0 dt))
