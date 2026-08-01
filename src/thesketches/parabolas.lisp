(sketches:def-sketch-package parabolas)
(in-package kg.sketch.parabolas)

;; The idea of this sketch is to draw parabolas back and forth between
;; the left and right sides of the canvas, descending from top to bottom.

(defsketch para
    ((width 400)
     (height 800)
     (dx 5))
  (let ((pos (vec2 0 0))
        (pal (get-palette
              (alexandria:random-elt (list :ch-pink-ice
                                           :palestine
                                           :sadiq
                                           :ch-blood-ice
                                           :ch-beach)))))
    
    (shuffle-palette pal)
    (background (palette-pop-colour pal))
    (loop while (<= (vy pos) height)
          do (let* ((new-pos (vec2 (if (= 0 (vx pos)) width 0)
                                   (+ 40 (random 80) (vy pos))))
                    ;; Need 3 points to fully define the parabola.
                    (third-point
                      (let ((d (+ 0.2 (random 0.6))))
                        ;; Pick a random point between the left & right sides, shifted so
                        ;; that it's above the line between them (and the parabola won't be
                        ;; upside down).
                        (v+ (v-scale d pos)
                            (v-scale (- 1 d) new-pos)
                            (vec2 0 (- (+ 10 (random 30.0))))))))
               ;; Now need to calculate the coefficients of the parabola.
               (multiple-value-bind (a b c)
                   (compute-parabola-coefficients pos new-pos third-point)
                 (let ((dir (signum (- (vx new-pos) (vx pos)))))
                   (with-pen (make-pen :stroke (next-colour pal) :weight 10)
                     (apply #'polyline
                            (loop for x = (vx pos) then (+ x (* dir dx))
                                  while (if (< dir 0)
                                            (>= x 0)
                                            (<= x width))
                                  collect x
                                  collect (+ (* a x x) (* b x) c))))))
               (setf pos new-pos))))
  (stop-loop))

;; Parabola formula:
;;   y = ax^2 + bx + c
;; Sub in the 3 points and solve for a, b and c.
;;   ax1^2 + bx1 + c = y1  [1]
;;   ax2^2 + bx2 + c = y2  [2]
;;   ax3^3 + bx3 + c = y3  [3]
;; First, subtract [1] from [2] to get...
;;   a(x2^2 - x1^2) + b(x2 - x1) = y2 - y1,
;; or
;;   a(x2 + x1) + b = (y2 - y1)/(x2 - x1).  [4]
;; Subtract [1] from [3] to get...
;;   a(x3 + x1) + b = (y3 - y1)/(x3 - x1).  [5]
;; Now subtract [4] from [5] and solve for a.
;;   a((x3 + x1) - (x2 + x1)) = (y3 - y1)/(x3 - x1) - (y2 - y1)/(x2 - x1),
;;   a(x3 - x2) = ...,
;;   a = 1/(x3 - x2) * ...
;; Sub this value for 'a' back into [4] to get 'b'.
;; Then sub them both into [1] to get 'c'.
(defun compute-parabola-coefficients (p1 p2 p3)
  ;; Reusing the slope calculation... for efficiency, but also because
  ;; it's verbose.
  (let* ((slope-1-to-2
           (/ (- (vy p2) (vy p1))
              (- (vx p2) (vx p1))))
         (a (* (/ (- (vx p3) (vx p2)))
               (- (/ (- (vy p3) (vy p1))
                     (+ (vx p3) (vx p1)))
                  slope-1-to-2)))
         (b (- slope-1-to-2 (* a (+ (vx p2) (vx p1)))))
         (c (- (vy p1)
               (* b (vx p1))
               (* a (square (vx p1))))))
    (values a b c)))
