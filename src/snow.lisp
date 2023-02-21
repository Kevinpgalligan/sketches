(in-package sketches)

(defstruct snowflake x y vx vy r)

(defsketch snowscene
    ((width 600)
     (height 500)
     ;;;; DEBUG STUFF
     (pause nil)
     (debug-points nil)
     ;;;; WIND STUFF.
     (wind-cell-size 50)
     ;; Change this to make the jumps in the wind values bigger/smaller
     ;; between cells.
     (wind-noise-scale 2.3)
     (N-wind (make-vnoise))
     (draw-wind-vectors-p nil)
     ;;;; SNOWFLAKE STUFF.
     (flake-max-velocity 5)
     (wind-flake-force 0.25)
     (flakes (loop repeat 20
                   collect (make-snowflake :x (random width)
                                           :y (random height)
                                           :vx 0
                                           :vy 0
                                           :r (+ 2 (random 3)))))
     ;;;; MOUNTAIN STUFF.
     ;; removed for debugging: 0 600 1200
     (peak-x-offsets '(1200))
     ;; removed for debugging: 100 200 300
     (peak-y-offsets '(300))
     (max-peak-height 150)
     (peak-colours (list
                    ;; removed for debugging:
                    ;(hsb-360 213 46 91)
                    ;(hsb-360 213 46 60)
                    (hsb-360 213 46 30)))
     ;; removed for debugging: 1 5 10
     (peak-dxs '(5))
     (peak-noise-scale 0.01)
     (peak-gap 20)
     (N-peaks (make-vnoise))
     ;; Time dimension.
     (z 0)
     (steps 0)
     (dz 0.01))
  (background +black+)
  (loop for peak-index = 0 then (1+ peak-index)
        for peak-colour in peak-colours
        for y-offset in peak-y-offsets
        for dx in peak-dxs
        for base-x = (* dx steps)
        do (let ((peak-points
                   (nconc
                    (loop for i = 0 then (1+ i)
                          for x-rel = (* i peak-gap)
                          while (<= x-rel width)
                          for x = (+ base-x x-rel)
                          collect x-rel
                          collect (+ y-offset (* max-peak-height (noise-get N-peaks (* peak-noise-scale x) peak-index))))
                    ;; Append the bottom right and bottom left corners as points so
                    ;; that we draw the mountain over the whole screen.
                    (list width height 0 height))))
             (with-pen (make-pen :fill peak-colour :stroke peak-colour)
               (apply #'polygon peak-points))
             ;; DEBUGGING
             (when pause
               (setf debug-points peak-points))
             (with-pen (make-pen :fill +red+ :stroke +red+)
               (loop for (x y) on peak-points by #'cddr
                     do (circle x y 5)))))
  (when draw-wind-vectors-p
    (dotimes (i (/ height wind-cell-size))
      (dotimes (j (/ width wind-cell-size))
        (let ((cx (+ (halve wind-cell-size) (* j wind-cell-size)))
              (cy (+ (halve wind-cell-size) (* i wind-cell-size)))
              (line-length (/ wind-cell-size 3))
              (angle (* 180 (noise-get N-wind (* wind-noise-scale i) (* wind-noise-scale j) z))))
          (with-current-matrix
            (rotate angle cx cy)
            ;; Draw a line straight down, so that when we rotate clockwise by ANGLE between
            ;; 0 degrees and 180 degrees, we get a line pointing somewhere to the left. 
            (with-pen (make-pen :fill +red+ :stroke +red+)
              (circle cx cy 2)
              (line cx cy cx (+ cy line-length))))))))
  (loop for flake in flakes
        do (with-pen (make-pen :fill +white+ :stroke +white+)
             (circle (snowflake-x flake) (snowflake-y flake) (snowflake-r flake)))
        do (incf (snowflake-x flake) (snowflake-vx flake))
        do (incf (snowflake-y flake) (snowflake-vy flake))
        do (if (outside-bounds-p flake width height)
               (setf (snowflake-x flake) (1- width)
                     (snowflake-y flake) (random height))
               (let* ((i (floor (/ (snowflake-y flake) wind-cell-size)))
                      (j (floor (/ (snowflake-x flake) wind-cell-size)))
                      ;; Angle points somewhere to the left.
                      (phi (+ (halve pi)
                              (* pi (noise-get N-wind (* wind-noise-scale i) (* wind-noise-scale j) z)))))
                 ;; Convert from polar coordinates (radius = 1, angle is phi) to
                 ;; cartesian (is that the name?) coordinates. All done manually, avoiding
                 ;; the decision of which vector library to use.
                 (let ((fx (* wind-flake-force (cos phi)))
                       (fy (* wind-flake-force (sin phi))))
                   (incf (snowflake-vx flake) fx)
                   (incf (snowflake-vy flake) fy)
                   ;; Scale coordinates so that magnitude of velocity
                   ;; does not exceed the max.
                   (with-slots (vx vy) flake
                     (let ((mag (sqrt (+ (square vx) (square vy)))))
                       (when (> mag flake-max-velocity)
                         (let ((factor (/ flake-max-velocity mag)))
                           (scalef (snowflake-vx flake) factor)
                           (scalef (snowflake-vy flake) factor)))))))))
  (when (not pause)
    (incf z dz)
    (incf steps)))

(defun outside-bounds-p (flake width height)
  (with-slots (x y r) flake
    (or (< (+ x r) 0)
        (< (+ y r) 0)
        (>= (- x r) width)
        (>= (- y r) height))))

;; DEBUGGING
(defmethod kit.sdl2:textinput-event ((window snowscene) ts text)
  (when (string= text "p")
    (when (slot-value window 'pause)
      ;; Unpausing, print out the values!
      (format t "~a~%" (slot-value window 'debug-points)))
    (setf (slot-value window 'pause) (not (slot-value window 'pause)))))
