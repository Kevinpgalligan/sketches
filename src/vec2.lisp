(in-package sketches)

(defun vec2 (x y)
  (list x y))

(defun vx (v) (car v))
(defun vy (v) (cadr v))

(defun (setf vx) (new-value v)
  (setf (car v) new-value))
(defun (setf vy) (new-value v)
  (setf (cadr v) new-value))

(defun v+ (v1 v2)
  (vec2 (+ (vx v1) (vx v2)) (+ (vy v1) (vy v2))))

(defun v- (v1 v2)
  (vec2 (- (vx v1) (vx v2)) (- (vy v1) (vy v2))))

(defun v-scale (a v)
  (vec2 (* a (vx v)) (* a (vy v))))

(defun v-length (v)
  (sqrt (+ (square (vx v)) (square (vy v)))))

(defun v-normalise (v)
  (let ((len (v-length v)))
    (if (zerop len)
        v
        (v-scale (/ 1 len) v))))

(defun v-rescale (new-length v)
  (v-scale new-length (v-normalise v)))

(defun v-clamp (max-length v)
  (let ((length (v-length v)))
    (if (< length max-length)
        v
        (v-rescale max-length v))))
