(sketches:def-sketch-package automata)
(in-package kg.sketch.automata)

;; TODO
;; 2. Interface for more easily creating automata (e.g. based on Wolfram number).
;; 3. 2d automata.

(defclass automaton-1d ()
  ((progress-fn :initarg :progress-fn :reader progress-fn)
   (states :initarg :states :reader states)
   (old-states :initarg :old-states :reader old-states)))

(defun make-1d-automaton (progress-fn &key size initial-states random-init num-possible-states init-fn)
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
  (let ((auto (make-1d-automaton
               (lambda (l c r)
                 ;; Rule 110.
                 (mod (+ c r (* c r) (* l c r)) 2))
               :random-init t
               :size num-states
               :num-possible-states 2))
        (row-height (/ height steps)))
    (draw-automaton-1d auto steps :width width :total-height height :palette-name :sadiq)
    (stop-loop)))
