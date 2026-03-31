(sketches:def-sketch-package automata)
(in-package kg.sketch.automata)

;; TODO
;; 1. Extract code for rendering an automaton. Including diff colours per state.
;; 2. Interface for more easily creating automata (e.g. based on Wolfram number).
;; 3. 2d automata.

(defclass automaton-1d ()
  ((next-state-fn :initarg :next-state-fn :reader next-state-fn)
   (states :initarg :states :reader states)
   (old-states :initarg :old-states :reader old-states)))

(defun make-1d-automaton (next-state-fn &key size initial-states random-init num-possible-states init-fn)
  (let ((states
          (cond
            (initial-states
             (make-array (length initial-states) :initial-contents initial-states))
            (random-init
             (when (not (and size num-possible-states))
               (error "Must provide size & number of possible states for random init."))
             (make-array size
                         :initial-contents (loop repeat size
                                                 collect (random num-possible-states))))
            (init-fn
             (when (not size)
               (error "Must provide size with init function."))
             (make-array size
                         :initial-contents (loop for i below size
                                                 collect (funcall init-fn i))))
            (t (error "Must provide initial-states, random-init or init-fn.")))))
    (make-instance 'automaton-1d
                   :next-state-fn next-state-fn
                   :states states
                   :old-states (copy-seq states))))

(defun automaton-1d-next-state (auto)
  (with-slots (next-state-fn states old-states) auto
    (let ((size (array-total-size states)))
      (loop for i below size
            do (setf (aref old-states i)
                     (funcall next-state-fn
                              (aref states (mod (1- i) size))
                              (aref states i)
                              (aref states (mod (1+ i) size)))))
      (rotatef states old-states))))

(defun automaton-1d-get (auto i)
  (with-slots (states) auto
    (aref states i)))

(defsketch automata
    ((width 400)
     (height 400)
     (num-states 40)
     (steps 40)
     (cvs (make-canvas num-states 1)))
  (let ((auto (make-1d-automaton
               (lambda (l c r)
                 ;; Rule 110.
                 (mod (+ c r (* c r) (* l c r)) 2))
               :random-init t
               :size num-states
               :num-possible-states 2))
        (row-height (/ height steps)))
    (with-identity-matrix
      (loop repeat steps
            do (progn
                 (loop for i below num-states
                       do (canvas-paint-gray255 cvs i 0 (* (automaton-1d-get auto i) 255)))
                 (draw cvs :width width :height row-height :mag-filter :nearest :min-filter :nearest)
                 (translate 0 row-height)
                 (automaton-1d-next-state auto))))
    (stop-loop)))
