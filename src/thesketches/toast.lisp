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
  (labels ((apply-velocity! (p-slot old-p-slot drag-coeff)
             (let ((vel (v- (slot-value toast p-slot)
                            (slot-value toast old-p-slot))))
               (setf (slot-value toast p-slot)
                     (v+ (slot-value toast p-slot)
                         (v-scale (- 1 drag-coeff) vel)))))
           (apply-bounds! (pslot)
             (symbol-macrolet ((x (vx (slot-value toast pslot)))
                               (y (vy (slot-value toast pslot))))
               (setf x (alexandria:clamp x 0 width))
               (setf y (alexandria:clamp y 0 height)))))
    (let ((old-p1 (p1 toast))
          (old-p2 (p2 toast)))
      (apply-velocity! 'p1 'old-p1 drag-coeff)
      (apply-velocity! 'p2 'old-p2 drag-coeff)
      ;; Gravity
      (setf (p1 toast) (v+ (p1 toast) gravity)
            (p2 toast) (v+ (p2 toast) gravity))
      ;; Bounds (should do the thing where the old position is
      ;; set outside the bounds, so that there's a bounce).
      (apply-bounds! 'p1)
      (apply-bounds! 'p2)
      (when apply-blastoff-p
        (let ((direction (v- (vec2 (random width) height) (vec2 (/ width 2) 0))))
          (setf (p1 toast) (v+ (p1 toast) (v-rescale blastoff-dist direction)))))
      ;; Length constraint
      (let* ((gap (v- (p2 toast) (p1 toast)))
             (curr-length (v-length gap))
             (offset (/ (- (len toast) curr-length) 2)))
        (if (< curr-length (len toast))
            (progn
              (setf (p1 toast) (v+ (v-rescale offset gap) (p1 toast))
                    (p2 toast) (v- (v-rescale offset gap) (p2 toast)))
              (setf (p1 toast) (v- (v-rescale offset gap) (p1 toast))
                    (p2 toast) (v+ (v-rescale offset gap) (p2 toast))))))
      (setf (old-p1 toast) old-p1
            (old-p2 toast) old-p2))))

(defsketch murphys-law
    ((width 500)
     (height 500)
     (toast (make-stick (vec2 50 500) (vec2 100 500) 15))
     (gravity (vec2 0 -0.4))
     (drag-coeff 0.05)
     (rest-duration 2)
     (rest-start-time nil)
     (blastoff-dist 10)
     (y-axis :up))
  (background +black+)
  (when (and (not rest-start-time)
             (v= (old-p1 toast) (p1 toast))
             (v= (old-p2 toast) (p2 toast)))
    ;; Stationary.
    (setf rest-start-time (get-universal-time)))
  (let ((apply-blastoff-p nil))
    (when (and rest-start-time (> (- (get-universal-time) rest-start-time)
                                  rest-duration))
      ;; Toast has rested long enough, time to fire it towards the ceiling.
      (setf apply-blastoff-p t)
      (setf rest-start-time nil))
    (update-state toast gravity drag-coeff width height
                  blastoff-dist apply-blastoff-p))
  (draw-toast toast))

