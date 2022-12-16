;; Wondering how to make it more life-like.
;; Maybe add wind, fog (based on Perlinesque noise, which varies
;; in time and 2d space). Wind can just be a horizontal effect on
;; the value of x that varies in time.

(in-package sketches)

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
  (set-and-drop-oldest! drop ny 'y 'old-ys))

(defparameter +old-vals-to-track+ 2)

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
     (dy 3)
     (drops (loop repeat 500
                  collect (make-drop (random width) (random height) (random depth)))))
  (background +black+)
  (loop for drop in drops
        do (let ((colour (hsb 0 0 (/ (z drop) depth)) +white+))
             (with-pen (make-pen :weight 1 :stroke colour :fill colour)
               (line (drop-oldest-x drop) (drop-oldest-y drop) (x drop) (y drop))))
           ;; Positive addition to y -> drop falls down.
        do (update-drop! drop (x drop) (+ (y drop) dy))
        do (when (drop-off-screen? drop height)
             (reinit-drop! drop width height depth))))
