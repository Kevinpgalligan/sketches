(sketches:def-sketch-package automata)
(in-package kg.sketch.automata)

"
Two ways of making the same 1d automaton.
  (make-1d-automaton-from-outputs 2 '(0 1 1 1 0 1 1 0) :random-init t :size num-states)
  (make-1d-automaton-from-wolfram-number 110 :random-init t :size num-states)
"

(defclass automaton-1d ()
  ((progress-fn :initarg :progress-fn :reader progress-fn)
   (states :initarg :states :reader states)
   (old-states :initarg :old-states :reader old-states)))

(defun make-1d-automaton (progress-fn &key size initial-states random-init num-possible-states init-fn &allow-other-keys)
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
                   :progress-fn progress-fn
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

(defun automaton-1d-progress (auto)
  (with-slots (progress-fn states old-states) auto
    (let ((size (array-total-size states)))
      (loop for i below size
            do (setf (aref old-states i)
                     (funcall progress-fn
                              (aref states (mod (1- i) size))
                              (aref states i)
                              (aref states (mod (1+ i) size)))))
      (rotatef states old-states))))

(defun automaton-1d-get (auto i)
  (with-slots (states) auto
    (aref states i)))

(defun automaton-1d-num-states (auto)
  (array-total-size (states auto)))

(defun draw-automaton-1d (auto steps &key width total-height (row-height 1) (x 0) (y 0) colour-fn palette-name)
  "COLOUR-FUN maps a cell state (integer) to a colour. By default, comes up with a reasonable colour for each."
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
                     do (canvas-paint cvs (funcall colour-fn (automaton-1d-get auto i)) i 0))
               (draw cvs :width width :height row-height :x x :y y :mag-filter :nearest :min-filter :nearest)
               (incf y row-height)
               (automaton-1d-progress auto)))))

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

(defsketch automata
    ((width 400)
     (height 400)
     (num-states 40)
     (steps 40)
     (cvs (make-canvas num-states 1)))
  (let ((auto (make-random-1d-automaton 3 :random-init t :size num-states)))
    (draw-automaton-1d auto steps :width width :total-height height :palette-name :sadiq)
    (stop-loop)))

(defmethod on-key ((instance automata) (key (eql :space)) (state (eql :up)))
  (start-loop))
