(sketches:def-sketch-package rain)
(in-package kg.sketch.rain)

(defclass drop ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)
   (z
    :initarg :z
    :accessor z)
   (old-xs
    :initarg :old-xs
    :accessor old-xs)
   (old-ys
    :initarg :old-ys
    :accessor old-ys)))

(defun drop-oldest-y (drop)
  (car (last (old-ys drop))))

(defun drop-oldest-x (drop)
  (car (last (old-xs drop))))

(defun drop-off-screen? (drop height)
  (> (drop-oldest-y drop) height))

(defun reinit-drop! (drop width height depth)
  (setf (x drop) (random width)
        (y drop) 0
        (z drop) (random depth)
        (old-xs drop) (list (x drop))
        (old-ys drop) (list (y drop))))

(defun make-drop (x y z)
  (make-instance 'drop :x x :y y :z z :old-xs (list x) :old-ys (list y)))

(defun update-drop! (drop nx ny)
  (set-and-drop-oldest! drop nx 'x 'old-xs)
  (set-and-drop-oldest! drop ny 'y 'old-ys)
  (when (< (x drop) (car (last (old-xs drop))))
    (loop for sublist on (old-xs drop)
          ;; Drop has looped to other side of screen, set the old
          ;; values to 0.
          when (< (x drop) (car sublist))
            do (setf (car sublist) 0))))

(defparameter +old-vals-to-track+ 3)

(defun set-and-drop-oldest! (drop new-val val-name old-vals-name)
  (push (slot-value drop val-name) (slot-value drop old-vals-name))
  (setf (slot-value drop val-name) new-val)
  (if (> (length (slot-value drop old-vals-name)) +old-vals-to-track+)
      (setf (slot-value drop old-vals-name)
            (butlast (slot-value drop old-vals-name)))))

(defsketch rain
    ((width 600)
     (height 400)
     (depth 300)
     (dy 2)
     (min-rain-weight 0.5)
     (max-rain-weight 2)
     (N-fog (make-perlin-noise 3))
     (fog-tile-interval 40)
     (fog-tile-size 40)
     (fog-alpha 0.3)
     (fog-noise-scale 0.1)
     (max-fog 0.8)
     (N-wind (make-perlin-noise 1))
     ;; Wind changes over time.
     (t0 0)
     (dt 0.006)
     (wind-threshold 0.4)
     (max-wind 5)
     (drops (loop repeat 1000
                  collect (make-drop (random width)
                                     (random height)
                                     (random depth)))))
  (background +black+)
  (loop for drop in drops
        for wind = (noise-get N-wind t0)
        for dx = (if (> wind wind-threshold)
                     (* (/ (- wind wind-threshold) (- 1 wind-threshold))
                        max-wind)
                     0)
        do (let ((colour (hsb 0.6 0.8 (/ (z drop) depth)) +white+))
             (with-pen (make-pen :weight (lerp min-rain-weight max-rain-weight (/ (z drop) depth))
                                 :stroke colour
                                 :fill colour)
               (line (drop-oldest-x drop) (drop-oldest-y drop) (x drop) (y drop))))
           ;; Positive addition to y -> drop falls down.
        do (update-drop! drop (mod (+ (x drop) dx) width) (+ (y drop) dy))
        do (when (drop-off-screen? drop height)
             (reinit-drop! drop width height depth)))
  (dotimes (i (/ height fog-tile-interval))
    (dotimes (j (/ width fog-tile-interval))
      (let* ((fog (noise-get N-fog (* fog-noise-scale i) (* fog-noise-scale j) t0))
             (colour (hsb 0 0 (* max-fog fog) fog-alpha))
             (stroke-colour (hsb 0 0 0 0)))
        (with-pen (make-pen :stroke stroke-colour :fill colour)
          (rect (* j fog-tile-interval)
                (* i fog-tile-interval)
                fog-tile-size
                fog-tile-size)))))
  (incf t0 dt))
