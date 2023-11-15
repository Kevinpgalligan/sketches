(sketches:def-sketch-package snow)
(in-package kg.sketch.snow)

(defstruct snowflake x y vx vy r)

(defsketch snow
    ((width 600)
     (height 500)
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
     (peak-y-offsets '(100 200 300))
     (max-peak-height 150)
     (peak-colours (list
                    (hsb-360 213 46 91)
                    (hsb-360 213 46 60)
                    (hsb-360 213 46 30)))
     (peak-dxs '(1 3 5))
     (peak-noise-scale 0.01)
     (peak-gap 20)
     (first-xs '(0 0 0))
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
        for rel-x-start in first-xs
        do (let ((peak-points
                   (nconc
                    (loop for i = 0 then (1+ i)
                          for x-rel = (+ rel-x-start (* i peak-gap))
                          for x = (+ base-x x-rel)
                          collect x-rel
                          collect (+ y-offset (* max-peak-height (noise-get N-peaks (* peak-noise-scale x) peak-index)))
                          while (<= x-rel width))
                    ;; Append the bottom right and bottom left corners as points so
                    ;; that we draw the mountain over the whole screen.
                    (list width height 0 height))))
             (with-pen (make-pen :fill peak-colour)
               (apply #'polygon peak-points))))
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
  (incf z dz)
  (incf steps)
  (loop for first-x on first-xs
        for dx in peak-dxs
        do (decf (car first-x) dx)
        when (<= (car first-x) (- peak-gap))
          do (incf (car first-x) peak-gap)))

(defun outside-bounds-p (flake width height)
  (with-slots (x y r) flake
    (or (< (+ x r) 0)
        (< (+ y r) 0)
        (>= (- x r) width)
        (>= (- y r) height))))
