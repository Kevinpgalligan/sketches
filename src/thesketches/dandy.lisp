;; Based on a drawing by my cousin Andrew.

(sketches:def-sketch-package dandy)
(in-package kg.sketch.dandy)

(defsketch dandy
    ((width 300)
     (height 300)
     (max-dist (truncate width 2))
     (n 7)
     (h 100)
     (num-petals 3)
     (max-petal-dist 70)
     (ypen (make-pen :fill +yellow+))
     (gpen (make-pen :stroke +green+ :weight 4))
     (rpen (make-pen :stroke +red+ :weight 2))
     (yellowy-r 5)
     (canvas (make-canvas width height))
     (spinner-center
      (vec2 (* .8 (- (random width) max-dist))
            (* .8 (- (random height) max-dist))))
     (spinner-offset 10)
     (spinner-radius 30)
     (spinner-angle 0)
     (spinner-angle-change (* 0.001 2 pi))
     (pen-angle 0)
     (spin-speed (* 0.015 2 pi))
     (radius-reduction 0.02)
     (t0 0)
     (dt 0.01))
  (background +white+)
  (translate max-dist max-dist)

  (let* ((spinner-pos
           (v+ spinner-center
               (vec2 (* spinner-offset (cos spinner-angle))
                     (* spinner-offset (sin spinner-angle)))))
         (pen-pos
           (v+ spinner-pos
               (vec2 (* spinner-radius (cos pen-angle))
                     (* spinner-radius (sin pen-angle))))))
    (with-drawing-to-canvas (canvas)
      (with-pen (:stroke +black+)
        (draw-arc spinner-pos pen-pos spin-speed :anticlockwise nil))))
  ;; Draw from top-left corner, accounting for translation.
  (draw canvas :x (- max-dist) :y (- max-dist))

  (dotimes (i n)
    (let* ((angle (* (/ i n) 2 pi))
           (end-x (* h (cos angle)))
           (end-y (* h (sin angle)))
           (c (vec2 0 0))
           (dir (vec2 end-x end-y))
           (num-pts 20))
      (noise-seed i)
      (noise-detail :lod 1)
      (let ((pts (cons c
                       (loop for j from 0 below num-pts
                             for pt = (v-scale (/ (1+ j) num-pts) dir)
                             collect (v+ (v-rescale!
                                          (* 100
                                             (- (noise (* 0.02 (v-length pt)) t0)
                                                0.5))
                                          (perpendicular-anticlockwise dir))
                                         pt)))))
        (with-pen gpen
          (apply #'polyline
                 (loop for pt in pts
                       collect (vx pt)
                       collect (vy pt))))
        (loop for arc-pt-scale in '(0.15 0.3 0.6 0.9)
              for arc-angle in (list (/ (* 3 pi) n)
                                     (/ (* 2 pi) n)
                                     (/ (* 1.25 pi) n)
                                     (/ (* 1 pi) n))
              do (let* ((last-pt (elt pts (1- num-pts)))
                        (second-last-pt (elt pts (- num-pts 2)))
                        (arc-pt (v+ last-pt
                                       (v-rescale!
                                        (* (v-length (v- c dir))
                                           arc-pt-scale)
                                        (v- last-pt second-last-pt)))))
                   (with-pen rpen
                     (draw-arc arc-pt last-pt arc-angle)
                     (draw-arc arc-pt last-pt arc-angle :anticlockwise t))
                   (with-pen ypen
                     (let ((yellow-center
                             (v+ last-pt 
                                 (v-rescale! (halve yellowy-r)
                                             (v- last-pt second-last-pt)))))
                       (circle (vx yellow-center)
                               (vy yellow-center)
                               yellowy-r))))))))

  (incf spinner-angle spinner-angle-change)
  (incf pen-angle spin-speed)
  (decf spinner-radius radius-reduction)
  (incf t0 dt))

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
