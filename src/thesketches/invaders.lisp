;;; Recurse Center interview
;;; Space Invaders

(sketches:def-sketch-package invaders)
(in-package kg.sketch.invaders)

(defparameter *player-width* 40)
(defparameter *player-height* 5)

(defclass positionable ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defclass shapeable ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defclass alien (positionable shapeable)
  ())

(defclass player (positionable shapeable)
  ((moving-left
    :initarg :moving-left
    :accessor moving-left)
   (moving-right
    :initarg :moving-right
    :accessor moving-right)))

(defclass bullet (positionable shapeable)
  ())

(defun make-alien (&key (x 0) (y 0))
  (make-instance 'alien :x x :y y :width 20 :height 20))

(defun make-player (&key (x 0) (y 0))
  (make-instance 'player :x x :y y
                         :width *player-width* :height *player-height*
                         :moving-left nil
                         :moving-right nil))

(defun make-bullet (&key (x 0) (y 0))
  (make-instance 'bullet :x x :y y :width 5 :height 5))

(defun draw-alien (alien)
  (with-pen (make-pen :fill +green+)
    (let ((radius (/ (width alien) 2)))
      (circle (+ radius (x alien))
              (+ radius (y alien))
              radius))))

(defun draw-player (player)
  (with-pen (make-pen :fill +indigo+)
    (rect (x player) (y player) (width player) (height player))))

(defun draw-bullet (bullet)
  (with-pen (make-pen :fill +white+)
    (rect (x bullet) (y bullet) (width bullet) (height bullet))))

(defsketch invaders
    ((width 400)
     (height 600)
     (aliens-per-row 5)
     (alien-offset 30)
     (horizontal-gap 40)
     (vertical-gap 40)
     (alien-dir :right)
     (alien-speed 1)
     (aliens
      (loop repeat 15
            for i = 0 then (1+ i)
            collect (make-alien :x (+ alien-offset
                                      (* (mod i aliens-per-row)
                                         horizontal-gap))
                                :y (+ alien-offset
                                      (* (floor i aliens-per-row)
                                         vertical-gap)))))
     (bullets (list))
     (bullet-speed 5)
     (player-speed 3)
     (player-offset 20)
     (player (make-player :x (- (/ width 2)
                                (/ *player-width* 2))
                          :y (- height player-offset *player-height*))))
  (background +black+)

  ;;; Update state.
  (loop for alien in aliens
        when (< (x alien) 0)
          do (progn (setf alien-dir :right)
                    (return))
        when (>= (+ (x alien) (width alien)) width)
          do (progn (setf alien-dir :left)
                    (return)))
  (loop for alien in aliens
        do (incf (x alien)
                 (funcall (if (eq alien-dir :right) #'+ #'-)
                          alien-speed)))
  (when (and (moving-left player) (>= (x player) player-speed))
    (decf (x player) player-speed))
  (when (and (moving-right player) (<= (+ (x player) (width player)) width))
    (incf (x player) player-speed))
  (loop for bullet in bullets
        do (decf (y bullet) bullet-speed))
  (setf bullets
        (remove-if (lambda (bullet) (< (y bullet) 0)) bullets))
  (loop for bullet in bullets
        do (loop for alien in aliens
                 do (when (collision? bullet alien)
                      (format t "Collision~%"))))

  ;;; Draw.
  (loop for alien in aliens
        do (draw-alien alien))
  (draw-player player)
  (loop for bullet in bullets
        do (draw-bullet bullet)))

(defun collision? (obj1 obj2)
  (and (or (and (< (x obj1) (x obj2))
                (< (x obj2) (+ (width obj1) (x obj1))))
           (and (< (x obj2) (x obj1))
                (< (x obj1) (+ (width obj2) (x obj2)))))
       (or (and (< (y obj1) (y obj2))
                (< (y obj2) (+ (height obj1) (y obj1))))
           (and (< (y obj2) (y obj1))
                (< (y obj1) (+ (height obj2) (y obj2)))))))

(defmethod on-key ((instance invaders) key state)
  (with-slots (player bullets) instance
    (cond
      ((eq key :left)
       (setf (moving-left player) (eq state :keydown)))
      ((eq key :right)
       (setf (moving-right player) (eq state :keydown)))
      ((eq key :space)
       (push (make-bullet :x (+ (x player) (/ (width player) 2))
                          :y (- (y player) (height player)))
             bullets)))))
