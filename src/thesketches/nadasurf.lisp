;;;; Based on Nada Surf's "Let Go" album cover.

(sketches:def-sketch-package nadasurf)
(in-package kg.sketch.nadasurf)

(defclass ball ()
  ((colour :initarg :colour :accessor colour)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (base-y :initarg :base-y :accessor base-y)
   (vel :initarg :vel :accessor vel)
   (accel :initarg :accel :accessor accel)
   (ticks :initarg :ticks :initform 0 :accessor ticks)))

(defun make-ball (colour x y)
  (make-instance 'ball
                 :colour colour
                 :x x
                 :y y
                 :base-y y
                 :vel 0
                 :accel 0
                 :ticks 0))

(defparameter *gravity* -0.1)

(defsketch nadasurf
    ((width 500)
     (height 500)
     (ball-max-y 25)
     (bg (make-bg width height))
     (radius 5)
     (restart-on-change nil)
     (ball-colours
      (mapcar #'hex-to-color
              '("#C92210" "#EDBC19" "#2C2783"
                "#272E4B" "#D45727" "#D95179"
                "#DCD5E7" "#F6C660" "#E79621"
                "#3F5DA3" "#8973B4" "#1A1D3C")))
     (n-colours (length ball-colours))
     (ballz (loop repeat 1000
                  collect (make-ball
                           (nth (random n-colours) ball-colours)
                           (random width)
                           (random ball-max-y))))
     (y-axis :up)
     (t0 0))
  (draw bg)
  (with-font (make-font :color +white+ :size 40)
    (text "NADA SURF" 150 320))
  (loop for ball in ballz
        for i from 0
        do (progn
             (let ((offset-y
                     (+ (base-y ball)
                        ;; Exponential ensures that, the lower the ball, the less
                        ;; effect the noise has on it, so that there's no empty
                        ;; space beneath the balls. The ones near the bottom won't
                        ;; be offset very much.
                        (* (exp (* 0.12 (- (base-y ball) ball-max-y)))
                           300
                           (noisy:noise (* 0.012 (x ball)) t0)))))
               ;; State update.
               (incf (y ball) (vel ball))
               (incf (vel ball) (accel ball))
               ;; Randomly give an acceleration to a few balls on every frame.
               (if (> (ticks ball) 0)
                   (progn
                     (decf (ticks ball))
                     (setf (accel ball) *gravity*))
                   (when (= 0 (random 700))
                     (setf (ticks ball) 60)
                     (setf (accel ball) 0.5)))
               (when (< (y ball) offset-y)
                 (setf (y ball) offset-y
                       (vel ball) 0
                       (accel ball) 0))
               (with-pen (:fill (colour ball))
                 (circle (x ball) (y ball) radius)))))
  (incf t0 0.003))

(defun make-bg (width height)
  (let ((bg-colour (rgb-255 85 114 148))
        (noise-scale 0.0213)
        (cvs (make-canvas width height)))
    (dotimes (i width)
      (dotimes (j height)
        (let ((b (* 0.3
                    (noisy:noise (* i noise-scale)
                                 (* j noise-scale))
                    ;; Attempt to add a "stippling" pattern.
                    (if (= (random 5) 0) 0.3 1))))
          (canvas-paint cvs
                        (color-filter-hsb bg-colour :brightness b)
                        i
                        j))))
    cvs))
