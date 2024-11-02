(sketches:def-sketch-package dandy)
(in-package kg.sketch.dandy)

(defsketch dandy
    ((width 500)
     (height 500)
     (n 7)
     (h 100)
     (num-petals 3)
     (max-petal-dist 70)
     (ypen (make-pen :fill +yellow+))
     (gpen (make-pen :stroke +green+ :weight 2))
     (rpen (make-pen :stroke +red+))
     (yellowy-r 5))
  (background +white+)
  (translate (/ width 2) (/ height 2))
  (dotimes (i n)
    (let* ((angle (* (/ i n) 2 pi))
           (end-x (* h (cos angle)))
           (end-y (* h (sin angle)))
           (c (vec2 0 0))
           (e (vec2 end-x end-y))
           (yellow-center (v+ e (v-rescale yellowy-r e))))
      (with-pen gpen
        (line 0 0 end-x end-y))
      (dotimes (j num-petals)
        (let ((arc-pt (v-scale (+ 1 (* (1+ j) 0.2)) (v- e c))))
          (with-pen rpen
            (draw-arc arc-pt e (/ (* 2 pi) n))
            (draw-arc arc-pt e (/ (* 2 pi) n) :anticlockwise t))
          (with-pen ypen
            (circle (vx yellow-center) (vy yellow-center) yellowy-r)))))))

;; Copying this over from reuleaux.lisp, need to extract it elsewhere.
;; (Except here, rather than giving the two endpoints of the arc, we accept
;;  the start point and an angle).
(defun make-arc (c start angle &key (steps 20) anticlockwise)
  (let* ((dir (v->polar! (v- start c)))
         (th (v-theta dir))
         (r (vr dir))
         (m (if anticlockwise 1 -1)))
    (loop for i from 0 upto steps
          for pt-angle = (+ th (* m (/ i steps) angle))
          collect (+ (vx c) (* r (cos pt-angle)))
          collect (+ (vy c) (* r (sin pt-angle))))))

(defun draw-arc (c pt angle &key (steps 20) anticlockwise)
  (apply #'polyline (make-arc c pt angle :steps steps :anticlockwise anticlockwise)))
