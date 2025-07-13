(sketches:def-sketch-package automata)
(in-package kg.sketch.automata)

;; Cellular automaton with M states and N neighbours is defined by mapping:
;;    0    -> x0
;;    1    -> x1
;;         ...
;;    N^M  -> x_{N^M},
;; where x_{i} is one of the M states.
(defclass cellular-automaton ()
  ((states :initarg :states :reader states)
   (next-state-fn :initarg :next-state-fn :reader next-state-fn)
   (grid :initarg :grid :reader grid)
   (old-grid :initarg :old-grid :reader old-grid)))

(defmacro do-grid ((grid indexes) &body body)
  (alexandria:with-gensyms (i)
    `(loop with ,indexes = (loop repeat (array-rank ,grid)
                                 collect 0)
           do (progn
                ,@body)
           do (bump-indexes ,indexes (array-dimensions ,grid))
           while (not
                  (loop for ,i in ,indexes
                        thereis (not (zerop ,i)))))))

(defun bump-indexes (indexes max-values)
  (loop for i on indexes
        for m in max-values
        do (progn
             (incf (car i))
             (if (>= (car i) m)
                 (setf (car i) 0)
                 (return))))
  indexes)

(defun make-cellular-automaton (states grid-size next-state-fn
                                &key initial-states random-init)
  (let ((grid (make-array grid-size :initial-contents initial-states)))
    (when random-init
      (do-grid (grid indexes)
        (apply #'(setf aref) (random states) grid indexes)))
    (make-instance 'cellular-automaton
                   :states states
                   :next-state-fn next-state-fn
                   :grid grid
                   :old-grid (make-array grid-size :initial-element 0))))

(defsketch automata
    ()
  )
