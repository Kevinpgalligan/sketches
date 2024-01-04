(sketches:def-sketch-package lorenz)
(in-package kg.sketch.lorenz)

;; See: 
;;  - https://thecodingtrain.com/challenges/12-lorenz-attractor
;;  - https://en.wikipedia.org/wiki/Lorenz_system

(defsketch lorenz
    ((width 400)
     (height 400)
     (x 2)
     (y 2)
     (z 1)
     (copy-pixels t)
     (y-axis :up)
     (dt 0.01)
     (sigma 10)
     (rho 28)
     (beta (/ 8 3))
     (z-scale 25)
     (reset-p nil))
  (if reset-p
      (progn (background +black+)
             (setf reset-p nil))
      (with-identity-matrix
        (translate (/ width 2) (/ height 2))
        (scale 3 3)
        (let ((px x)
              (py y)
              (pz z))
          (incf x (* dt sigma (- py px)))
          (incf y (* dt (- (* px (- rho pz)) py)))
          (incf z (* dt (- (* px py) (* beta pz))))
          (with-pen (make-pen :stroke (lerp-color +blue+
                                                  +red+
                                                  (/ (+ z-scale
                                                        (alexandria:clamp z (- z-scale) z-scale))
                                                     (* 2 z-scale)))
                              :weight 0.3)
            (line px py x y))))))

(defmethod on-key ((instance lorenz) key state)
  (with-slots (reset-p x y z sigma rho beta) instance
    (when (and (eq key :space) (eq state :keydown))
      (setf reset-p t
            x (random 5.0)
            y (random 5.0)
            z (random 5.0)
            sigma (random 50.0)
            rho (random 50.0)
            beta (random 2.0)))))
