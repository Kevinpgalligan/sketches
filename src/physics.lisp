(in-package sketches)

;; References:
;;   Coding Train soft body physics:
;;        https://www.youtube.com/watch?v=IxdGyqhppis
;;   Soft body physics paper:
;;        https://www.cs.cmu.edu/afs/cs/academic/class/15462-s13/www/lec_slides/Jakobsen.pdf
;;   Jelly car explainer:
;;        https://www.youtube.com/watch?v=3OmkehAJoyo

;; TODO
;;   3. add different forces.
;;      drag (?), gravity, bounds, stick, spring
;;      not sure yet how to order them in a generic way...
;;   4. (physics-step world) or maybe (update-state world)
;;   5. how does user interact with particles?
;;      e.g. dragging particle with mouse
;;           or applying blastoff in toast sketch
;;   6. equivalent of "locking" a particle and being able to update its state?
;;      not sure what that means.
;;   7. test toast & space invaders sketches, I think they might be broken

(defclass world ()
  ((particles :initarg :initarg
              :initform nil
              :accessor particles)))

(defun make-world ()
  (make-instance 'world))

(defun add-particle (world particle)
  (push particle (particles world))))

(defclass verlet-particle ()
  ((pos
    :initarg :pos
    :accessor pos)
   (old-pos
    :initarg :old-pos
    :accessor old-pos)))

(defun make-verlet-particle (&key (pos (vec2 0 0)))
  (make-instance 'verlet-particle
                 :pos pos
                 :old-pos pos))
