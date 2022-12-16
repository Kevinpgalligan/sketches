(in-package sketches)

(defun lerp (x1 x2 t0)
  "Linear interpolation between x1 and x2 given weight t0, 0<=t0<=1."
  (+ (* (- 1 t0) x1) (* t0 x2)))

(defun smoothstep (t0)
  (* t0 t0 (- 3 (* 2 t0))))
