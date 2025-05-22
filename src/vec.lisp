(in-package sketches)

(defun vec (&rest values)
  (make-array (list (length values)) :initial-contents values))

(defun zeros-vec (size)
  (make-array (list size) :initial-element 0))

(defun vec2 (x y)
  (make-array '(2) :initial-contents (list x y)))

(defun vec3 (x y z)
  (make-array '(3) :initial-contents (list x y z)))

(defun vec3f (x y z)
  (make-array '(3)
              :element-type 'single-float
              :initial-contents (list (float x)
                                      (float y)
                                      (float z))))

(defun vx (v) (aref v 0))
(defun vy (v) (aref v 1))
(defun vz (v) (aref v 2))
(defun vn (v i) (aref v i))
(defun vr (v) (aref v 0))
(defun v-theta (v) (aref v 1))

(defun v-copy (v)
  (copy-seq v))

(defun v= (v1 v2)
  (loop for x1 across v1
        for x2 across v2
        always (= x1 x2)))

(defun (setf vx) (new-value v)
  (setf (aref v 0) new-value))
(defun (setf vy) (new-value v)
  (setf (aref v 1) new-value))
(defun (setf vz) (new-value v)
  (setf (aref v 2) new-value))
(defun (setf vn) (new-value v i)
  (setf (aref v i) new-value))
(defun (setf vr) (new-value v)
  (setf (aref v 0) new-value))
(defun (setf v-theta) (new-value v)
  (setf (aref v 1) new-value))

(defmacro def-vop (name operator)
  (alexandria:with-gensyms (v1 vs i val other-v)
    
    `(defun ,name (,v1 &rest ,vs)
       (apply #'vec
              (loop for ,i from 0 below (length ,v1)
                    collect (let ((,val (vn ,v1 ,i)))
                              (loop for ,other-v in ,vs
                                    do (setf ,val (,operator ,val (vn ,other-v ,i))))
                              ,val))))))

(defmacro def-vop! (name operator)
  (alexandria:with-gensyms (v1 vs i other-v)
    `(defun ,name (,v1 &rest ,vs)
       (loop for ,i from 0 below (length ,v1)
             do (loop for ,other-v in ,vs
                      do (setf (vn ,v1 ,i)
                               (,operator (vn ,v1 ,i)
                                          (vn ,other-v ,i))))))))

(defmacro def-vops (name name! operator)
  `(progn
     (def-vop ,name ,operator)
     (def-vop! ,name! ,operator)))

(def-vops v+ v+! +)
(def-vops v- v-! -)
(def-vops v* v*! *)
(def-vops v/ v/! /)

(defun v-scale (a v)
  (apply #'vec (loop for x across v collect (* a x))))

(defun v-scale! (a v)
  (loop for i from 0 below (length v)
        do (setf (vn v i) (* a (vn v i))))
  v)

(defun v-length (v)
  (sqrt (loop for x across v
              sum (square x))))

(defun euclidean-distance (v1 v2)
  (sqrt (loop for x across v1
              for y across v2
              sum (square (- x y)))))

(defun v-normalise (v)
  (let ((len (v-length v)))
    (if (zerop len)
        v
        (v-scale (/ 1 len) v))))

(defun v-normalise! (v)
  (let ((len (v-length v)))
    (if (zerop len)
        nil
        (v-scale! (/ 1 len) v))))

(defun v-rescale (new-length v)
  (let ((new-v (v-normalise v)))
    (v-scale! new-length new-v)
    new-v))

(defun v-rescale! (new-length v)
  (v-normalise! v)
  (v-scale! new-length v))

(defun v-clamp (max-length v)
  (let ((length (v-length v)))
    (if (< length max-length)
        v
        (v-rescale max-length v))))

(defun v-clamp! (max-length v)
  (let ((length (v-length v)))
    (if (< length max-length)
        nil
        (v-rescale! max-length v))))

(defun get-nearest-point-on-grid (v spacing)
  "Finds nearest point to a (2d) vector given a (2d) grid of points with a
distance of SPACING between each one, the point (0,0) is on the grid."
  (vec2 (round-to-nearest-multiple (vx v) spacing)
        (round-to-nearest-multiple (vy v) spacing)))

(defun get-pos-on-unit-circle (angle-radians)
  (vec2 (cos angle-radians) (sin angle-radians)))

(defun v-dot (v1 v2)
  (loop for x across v1
        for y across v2
        sum (* x y)))

(defun v-rotate (rad v)
  "Returns a 2d vector V rotated by RAD radians clockwise."
  (let ((c (cos (- rad)))
        (s (sin (- rad))))
    (vec2 (- (* c (vx v)) (* s (vy v)))
          (+ (* s (vx v)) (* c (vy v))))))

(defun perpendicular-anticlockwise (v)
  (vec2 (vy v) (- (vx v))))

(defun perpendicular-clockwise (v)
  (v-scale! -1 (perpendicular-anticlockwise v)))

(defun v->polar! (v)
  "Angle is from 0->2pi."
  (let ((x (vx v))
        (y (vy v)))
    (setf (vr v) (v-length v))
    (setf (v-theta v) (atan y x))
    v))

(defun v->polar (v)
  (v->polar! (v-copy v)))

(defun v->cartesian! (v)
  (let ((r (vr v))
        (theta (v-theta v)))
    (setf (vx v) (* r (cos theta)))
    (setf (vy v) (* r (sin theta)))
    v))

(defun polar-vec (theta r)
  (v->cartesian! (vec2 theta r)))
