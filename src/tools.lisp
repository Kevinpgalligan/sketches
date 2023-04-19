(in-package sketches)

(defun lerp (x1 x2 t0)
  "Linear interpolation between x1 and x2 given weight t0, 0<=t0<=1."
  (+ (* (- 1 t0) x1) (* t0 x2)))

(defun smoothstep (t0)
  (* t0 t0 (- 3 (* 2 t0))))

(defun remap (x la ha lb hb)
  "Takes x from the interval [la, ha] and remaps it to the
interval [lb, hb]. If x is outside the expected interval, then
it gets clamped."
  (setf x (alexandria:clamp x la ha))
  (+ lb (* (- hb lb) (/ (- x la) (- ha la)))))

(defun halve (x)
  (/ x 2))

(defun square (x)
  (* x x))

(defmacro scalef (place factor)
  `(setf ,place (* ,place ,factor)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names
               collect (list name '(gensym)))
     ,@body))

(defmacro with-centered ((window-width window-height width height) &body body)
  (with-gensyms (x-offset y-offset)
    `(let ((,x-offset (halve (- ,window-width ,width)))
           (,y-offset (halve (- ,window-height ,height))))
       (with-identity-matrix
         (translate ,x-offset ,y-offset)
         ,@body))))

(defun round-to-nearest-multiple (x mul)
  (let ((remainder (rem x mul)))
    (if (< remainder (halve mul))
        (- x remainder)
        (+ x (- mul remainder)))))

(defun outside-range-p (low high x)
  (or (< x low) (< high x)))
