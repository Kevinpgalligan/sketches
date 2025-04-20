(in-package sketches)

;; TODO
;;   1. make-x functions for world & particles.
;;   2. (add-particle world verlet-particle)
;;   3. add different forces.
;;      drag (?), gravity, bounds, stick, spring
;;      not sure yet how to order them in a generic way...
;;   4. (physics-step world) or maybe (update-state world)
;;   5. how does user interact with particles?
;;      e.g. dragging particle with mouse
;;           or applying blastoff in toast sketch
;;   6. test toast & space invaders sketches, I think they might be broken

(defclass world ()
  ((particles :initarg :initarg
              :initform nil
              :accessor particles)))

(defclass verlet-particle ()
  ((pos
    :initarg :pos
    :accessor pos)
   (old-pos
    :initform pos
    :accessor old-pos)))

