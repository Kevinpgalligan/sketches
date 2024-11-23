;;;; Based on: http://www.kfish.org/boids/pseudocode.html

(sketches:def-sketch-package boids)
(in-package kg.sketch.boids)

(defclass boid ()
  ((pos :initarg :pos :accessor pos)
   (velocity :initarg :velocity
             :initform (vec2 0 0)
             :accessor velocity)))

(defun make-boid (x y)
  (make-instance 'boid :pos (vec2 x y)))

(defsketch boids
    ((width 400)
     (height 400)
     (restart-on-change nil)
     (boids (loop repeat 20
                  collect (make-boid (random width) (random height)))
            :tweakable t))
  (background +white+)
  (draw-boids boids)
  (update-positions boids))

(defun draw-boids (boids)
  (let ((boid-width 10)
        (boid-length 20))
    (loop for boid in boids
          do (with-slots (pos velocity) boid
               (with-pen (:fill +black+)
                 (let* ((dir (if (zerop (v-length velocity))
                                 (vec2 0 -1)
                                 (v-normalise velocity)))
                        (p1 (v+ pos (v-rescale (/ boid-length 2) dir)))
                        (p2 (v+ pos
                                (v-rescale (- (/ boid-length 2)) dir)
                                (v-rescale (/ boid-width 2)
                                           (perpendicular-anticlockwise dir))))
                        (p3 (v+ pos
                                (v-rescale (- (/ boid-length 2)) dir)
                                (v-rescale (/ boid-width 2)
                                           (perpendicular-clockwise dir)))))
                   (polygon (vx p1) (vy p1)
                            (vx p2) (vy p2)
                            (vx p3) (vy p3))))))))

(defun update-positions (boids)
  (let ((max-velocity 10))
    (map nil
         (lambda (boid)
           (setf (pos boid) (v+ (pos boid) (velocity boid))))
         boids)
    (loop for boid in boids
          do (setf (velocity boid)
                   (v-clamp max-velocity
                            (v+ (velocity boid)
                                (rule1 boid boids)
                                (rule2 boid boids)
                                (rule3 boid boids)))))))

(defun rule1 (boid boids)
  (let ((center (vec2 0 0)))
    (map nil
         (lambda (boid2)
           (when (not (eq boid boid2))
             (v+! center (pos boid2))))
         boids)
    (v-scale! (/ (1- (length boids))) center)
    (v-! center (pos boid))
    (v-scale! (/ 100) center)
    center))

(defun rule2 (boid boids)
  (let ((v-sum (vec2 0 0)))
   (loop for boid2 in boids
         for offset = (v- (pos boid) (pos boid2))
         for dist = (v-length offset)
         when (and (not (eq boid boid2)) (< dist 10))
           do (v+! v-sum offset))
   v-sum))

(defun rule3 (boid boids)
  (let ((result (vec2 0 0)))
    (map nil
         (lambda (boid2)
           (when (not (eq boid boid2))
             (v+! result (velocity boid2))))
         boids)
    (v-scale! (/ (1- (length boids))) result)
    (v-! result (velocity boid))
    (v-scale! (/ 8) result)
    result))
