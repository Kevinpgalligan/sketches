(sketches:def-sketch-package automata)
(in-package kg.sketch.automata)

"
EXAMPLES (for 1d automata).

Two ways of making the same 1d automaton.
  (make-1d-automaton-from-outputs 2 '(0 1 1 1 0 1 1 0) :random-init t :size num-states)
  (make-1d-automaton-from-wolfram-number 110 :random-init t :size num-states)

Full sketch body:
  (let ((auto (make-random-1d-automaton 3 :random-init t :size num-states)))
    (render-1d-automaton auto steps :width width :total-height height :palette-name :sadiq)
    (stop-loop))
"

(defclass automaton-1d ()
  ((transition-fn :initarg :transition-fn :reader transition-fn)
   (states :initarg :states :reader states)
   (old-states :initarg :old-states :reader old-states)))

(defun make-1d-automaton (transition-fn &key size initial-states random-init num-possible-states init-fn &allow-other-keys)
  "TRANSITION-FN takes 3 states as input: left neighbour state, current state, and right state."
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
                   :transition-fn transition-fn
                   :states states
                   :old-states (copy-seq states))))

(defun make-1d-automaton-from-wolfram-number (wolfnum &rest other-args &key &allow-other-keys)
  (apply #'make-1d-automaton
         (make-wolfram-transition-func wolfnum)
         :num-possible-states 2
         other-args))

(defun make-wolfram-transition-func (wolfnum)
  (when (not (<= 0 wolfnum 255))
    (error "Wolfram number must be from 0 to 255 (inclusive)."))
  (lambda (l c r)
    ;; Masks out the correct binary digit from the Wolfram number, indicating
    ;; the next state of this cell. For example, an input of l=1, c=1, r=0 would
    ;; correspond to a shift of 110b=6d, giving a mask of 01000000b.
    (if (zerop (logand wolfnum (ash 1 (+ (* 4 l) (* 2 c) r))))
        0
        1)))

(defun make-1d-automaton-from-outputs (num-possible-states outputs &rest other-args &key &allow-other-keys)
  "OUTPUTS is just a list of values, each of which must be 0 or greater and less than NUM-POSSIBLE-STATES."
  (let ((expected-num-outputs (expt num-possible-states 3))
        (actual-num-outputs (length outputs)))
    (when (not (= expected-num-outputs actual-num-outputs))
      (error (format nil "Expected ~a outputs but got ~a" expected-num-outputs actual-num-outputs))))
  (apply #'make-1d-automaton
         (let ((mappings (make-hash-table)))
           (loop for v in outputs
                 for i from 0
                 do (setf (gethash i mappings) v))
           (lambda (l c r)
             (gethash (+ (* l (square num-possible-states)) (* c num-possible-states) r)
                      mappings)))
         :num-possible-states num-possible-states
         other-args))

(defun make-random-1d-automaton (num-possible-states &rest other-args &key &allow-other-keys)
  (apply #'make-1d-automaton-from-outputs
         num-possible-states
         (loop repeat (expt num-possible-states 3)
               collect (random num-possible-states))
         other-args))

(defun transition-1d-automaton (auto)
  (with-slots (transition-fn states old-states) auto
    (let ((size (array-total-size states)))
      (loop for i below size
            do (setf (aref old-states i)
                     (funcall transition-fn
                              (aref states (mod (1- i) size))
                              (aref states i)
                              (aref states (mod (1+ i) size)))))
      (rotatef states old-states))))

(defun index-1d-automaton (auto i)
  (with-slots (states) auto
    (aref states i)))

(defun automaton-1d-num-states (auto)
  (array-total-size (states auto)))

(defun render-1d-automaton (auto steps &key width total-height (row-height 1) (x 0) (y 0) colour-fn palette-name)
  "Takes a 1D automaton and runs it for STEPS steps, drawing the states at each step.
TOTAL-HEIGHT determines the height of the drawing; if not provided, the total is derived from ROW-HEIGHT.
COLOUR-FUN maps a cell state (integer) to a colour. By default, comes up with a reasonable colour for each."
  (let* ((num-states (automaton-1d-num-states auto))
         (cvs (make-canvas num-states 1)))
    (when (null width)
      (setf width num-states))
    (when (null colour-fn)
      (setf colour-fn (if palette-name
                          (make-colour-func-from-palette palette-name :shuffle-palette t)
                          (make-default-colour-func))))
    (when total-height
      (setf row-height (/ total-height steps)))
    (loop repeat steps
          do (progn
               (loop for i below num-states
                     do (canvas-paint cvs (funcall colour-fn (index-1d-automaton auto i)) i 0))
               (draw cvs :width width :height row-height :x x :y y :mag-filter :nearest :min-filter :nearest)
               (incf y row-height)
               (transition-1d-automaton auto)))))

(defun make-default-colour-func ()
  (make-colour-func (lambda (s) (hash-color s))))

(defun make-colour-func-from-palette (palette-name &key shuffle-palette)
  (let ((pal (get-palette palette-name)))
    (when shuffle-palette
      (shuffle-palette pal))
    (make-colour-func
     (lambda (s)
       (declare (ignorable s))
       (next-colour pal)))))

(defun make-colour-func (colour-generate-fn)
  (let ((colour-map (make-hash-table)))
    (lambda (s)
      (or (gethash s colour-map)
          (let ((c (funcall colour-generate-fn s)))
            (setf (gethash s colour-map) c)
            c)))))

(defun state->black-or-white (state)
  (case state
    (0 +white+)
    (1 +black+)
    (t (error "This colour function only supports up to 2 states."))))

(defclass automaton-2d ()
  ((transition-fn :initarg :transition-fn :reader transition-fn)
   (states :initarg :states :reader states)
   (old-states :initarg :old-states :reader states)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (neighbour-fn :initarg :neighbour-fn :reader neighbour-fn)))

(defun moore-neighbours (states i j width height)
  (let ((tr (mod (1- i) height))  ; top row
        (br (mod (1+ i) height))  ; bottom
        (lc (mod (1- j) width))   ; left col
        (rc (mod (1+ j) width)))  ; right
    ;; Should probably pass in a list to save the results to, to avoid cons-ing so much.
    (list (aref states tr lc) (aref states tr j) (aref states tr rc)
          (aref states i lc)                     (aref states i rc)
          (aref states br lc) (aref states br j) (aref states br rc))))

(defun neumann-neighbours (states i j width height)
  (list                                    (aref states (mod (1- i) height) j)
        (aref states i (mod (1- j) width))                                     (aref states i (mod (1+ j) width))
                                           (aref states (mod (1+ i) height) j)))

(defun make-2d-automaton (transition-fn &key width height (neighbourhood :moore) initial-states random-init num-possible-states init-fn &allow-other-keys)
  "TRANSITION-FN takes the current state and a list of neighbour states as input.
INIT-FN takes row index and column index as input.
NEIGHBOURHOOD determines whether all 8 neighbouring cells are passed to the transition function (:moore), or just
  the 4 cells to north/south/east/west (:neumann)."
  (let ((states
          (cond
            (initial-states
             (make-array (list (length initial-states)
                               (length (first initial-states)))
                         :initial-contents initial-states))
            (random-init
             (when (not (and width height num-possible-states))
               (error "Must provide width & height & number of possible states for random init."))
             (make-array (list width height)
                         :initial-contents (loop repeat height
                                                 collect (loop repeat width
                                                               collect (random num-possible-states)))))
            (init-fn
             (when (not (and width height))
               (error "Must provide width & height with init function."))
             (make-array (list width height)
                         :initial-contents (loop for i below height
                                                 collect (loop for j below width
                                                               collect (funcall init-fn i j)))))
            (t (error "Must provide initial-states, random-init or init-fn.")))))
    (make-instance 'automaton-2d
                   :transition-fn transition-fn
                   :states states
                   :old-states (alexandria:copy-array states)
                   :width (array-dimension states 0)
                   :height (array-dimension states 1)
                   :neighbour-fn (case neighbourhood
                                   (:moore #'moore-neighbours)
                                   (:neumann #'neumann-neighbours)
                                   (t (error (format nil "Unknown neighbourhood parameter ~a" neighbourhood)))))))

(defun draw-2d-automaton (auto &key (x 0) (y 0) draw-width draw-height colour-fn)
  (when (null colour-fn)
    (setf colour-fn (get-palette :black-and-white)))
  (with-slots (states width height) auto
    (when (null draw-width)
      (setf draw-width width))
    (when (null draw-height)
      (setf draw-height height))
    (let ((cvs (make-canvas width height)))
      (loop for i below height
            do (loop for j below width
                     do (canvas-paint cvs (funcall colour-fn (aref states i j)) j i)))
      (draw cvs :x x :y y :width draw-width :height draw-height :min-filter :nearest :mag-filter :nearest))))

(defun transition-2d-automaton (auto)
  (with-slots (states old-states width height transition-fn neighbour-fn) auto
    (loop for i below height
          do (loop for j below width
                  do (setf (aref old-states i j)
                           (funcall transition-fn
                                    (aref states i j)
                                    (funcall neighbour-fn states i j width height)))))
    (rotatef states old-states)))

(defun game-of-life-transition (state neighbours)
  (let ((count (reduce #'+ neighbours)))
    (if (or (and (= 0 state) (= 3 count))
            (and (= 1 state) (<= 2 count 3)))
        1
        0)))

(defsketch automata
    ((width 200)
     (height 200)
     (auto (make-2d-automaton #'game-of-life-transition
                              :width width
                              :height height
                              :random-init t
                              :num-possible-states 2)))
  (draw-2d-automaton auto :colour-fn #'state->black-or-white)
  (transition-2d-automaton auto))

(defmethod on-key ((instance automata) (key (eql :space)) (state (eql :up)))
  (start-loop))
