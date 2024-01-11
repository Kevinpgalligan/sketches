;;;; Based on the prompt "Murphy's Law" in the Recurse Center's
;;;; Creative Coding group.
;;;; Basically contains a buggy implementation of verlet physics
;;;; for particles and sticks.

(sketches:def-sketch-package toast)
(in-package kg.sketch.toast)

(defclass stick ()
  ((p1 :initarg :p1 :accessor p1)
   (p2 :initarg :p2 :accessor p2)
   (len :initarg :len :accessor len)
   (old-p1 :initarg :old-p1 :accessor old-p1)
   (old-p2 :initarg :old-p2 :accessor old-p2)
   (width :initarg :width :accessor width)))

(defun make-stick (p1 p2 width)
  (make-instance 'stick
                 :p1 p1
                 :p2 p2
                 :len (v-length (v- p1 p2))
                 :old-p1 p1
                 :old-p2 p2
                 :width width))

(defun draw-toast (toast)
  (with-slots (p1 p2 width) toast
    (with-pen (make-pen :stroke (rgb-255 212 183 160) :weight width)
      (line (vx p1) (vy p1) (vx p2) (vy p2)))
    (with-pen (make-pen :stroke +red+ :weight (/ width 3))
      (let* ((gap (v- p2 p1))
             (perp (vec2 (- (vy p1) (vy p2))
                            (- (vx p2) (vx p1))))
             (jamwidth (/ width 3))
             (jam1 (v+ p1
                       (v+ (v-rescale jamwidth perp)
                           (v-rescale (* width 0.1) gap))))
             (jam2 (v+ p2
                       (v+ (v-rescale jamwidth perp)
                           (v-rescale (- (* width 0.1)) gap)))))
        (line (vx jam1) (vy jam1) (vx jam2) (vy jam2))))))

(defun update-state (toast gravity drag-coeff width height
                     blastoff-dist apply-blastoff-p)
  (labels ((zero-if-very-small (v)
             (if (< (v-length v) 0.1)
                 (vec2 0 0)
                 v))
           (apply-velocity! (p-slot old-p-slot drag-coeff)
             (let ((vel (zero-if-very-small
                         (v- (slot-value toast p-slot)
                             (slot-value toast old-p-slot)))))
               (setf (slot-value toast p-slot)
                     (v+ (slot-value toast p-slot)
                         (v-scale (- 1 drag-coeff) vel)))))
           (apply-bounds! (pslot)
             (symbol-macrolet ((x (vx (slot-value toast pslot)))
                               (y (vy (slot-value toast pslot))))
               (setf x (alexandria:clamp x 0 width))
               (setf y (alexandria:clamp y 0 height)))))
    (with-slots (p1 p2) toast
      (let ((old-p1 (v-copy p1))
            (old-p2 (v-copy p2)))
        (apply-velocity! 'p1 'old-p1 drag-coeff)
        (apply-velocity! 'p2 'old-p2 drag-coeff)
        ;; Gravity.
        (setf p1 (v+ p1 gravity)
              p2 (v+ p2 gravity))
        (when apply-blastoff-p
          (setf p1 (v+ p1 (v-rescale blastoff-dist
                                     (v- (vec2 (random width) height) p1))))
          (setf p2 (v+ p2 (v-rescale blastoff-dist
                                     (v- (vec2 (random width) height) p2)))))
        ;; Bounds.
        (apply-bounds! 'p1)
        (apply-bounds! 'p2)
        ;; Length constraint
        (let* ((gap (v- p2 p1))
               (curr-length (v-length gap))
               (offset (/ (- (len toast) curr-length) 2)))
          (when (not (= curr-length (len toast)))
            (setf p1 (v- p1 (v-rescale offset gap))
                  p2 (v+ p2 (v-rescale offset gap)))))
        (setf (old-p1 toast) (add-bounce old-p1 p1 width height)
              (old-p2 toast) (add-bounce old-p2 p2 width height))))))

(defun add-bounce (old-p p width height)
  (return-from add-bounce old-p) ; DEBUG
  (vec2
   (mirror-if-at-bound (vx old-p) (vx p) width)
   (mirror-if-at-bound (vy old-p) (vy p) height)))

(defun mirror-if-at-bound (old-x x bound)
  (cond
    ((= x 0) (- old-x))
    ((= x bound) (+ bound (- bound old-x)))
    (t old-x)))

(defun vec-down-p (v)
  (> (v-dot (vec2 0 -1) v) 0))

(defun going-down-p (toast)
  (or (vec-down-p (v- (p1 toast) (old-p1 toast)))
      (vec-down-p (v- (p2 toast) (old-p2 toast)))))

(defsketch murphys-law
    ((width 500)
     (height 500)
     (toast (make-stick (vec2 50 500) (vec2 100 500) 15))
     (gravity (vec2 0 -0.2))
     (drag-coeff 0.01)
     (rest-duration 2)
     (rest-start-time nil)
     (blastoff-dist 15)
     (danger-zone (/ width 4))
     (y-axis :up))
  (background +black+)
  (when (and (not rest-start-time)
             (v= (old-p1 toast) (p1 toast))
             (v= (old-p2 toast) (p2 toast)))
    ;; Stationary.
    (setf rest-start-time (get-universal-time)))
  (let ((apply-blastoff-p nil))
    (when (or (and rest-start-time
                   (> (- (get-universal-time) rest-start-time)
                      rest-duration))
              ;; In danger of not falling on the jam side!
              (and (or (< (vy (p1 toast)) danger-zone)
                       (< (vy (p2 toast)) danger-zone))
                   (going-down-p toast)
                   (< (vx (p1 toast)) (vx (p2 toast)))))
      ;; Toast has rested long enough, time to fire it towards the ceiling.
      (setf apply-blastoff-p t)
      (setf rest-start-time nil))
    (update-state toast gravity drag-coeff width height
                  blastoff-dist apply-blastoff-p))
  (draw-toast toast))
