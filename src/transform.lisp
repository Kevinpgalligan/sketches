(in-package sketches)

;; Linear transformations used for 3d camera math.
;; This was originally a shim over the sb-cga library, but that
;; only works with arrays of single-floats, and I'm going
;; for convenience over efficiency.

(defun matrix (x11 x12 x13 x14
               x21 x22 x23 x24
               x31 x32 x33 x34
               x41 x42 x43 x44)
  (make-array '(4 4)
              :initial-contents (list (list x11 x12 x13 x14)
                                      (list x21 x22 x23 x24)
                                      (list x31 x32 x33 x34)
                                      (list x41 x42 x43 x44))))

(defun %alloc-matrix ()
  (make-array '(4 4)))

(defun matrix* (m1 m2)
  (let ((result (%alloc-matrix)))
    (dotimes (i 4)
      (dotimes (j 4)
        (setf (aref result i j)
              (loop for k below 4
                    sum (* (aref m1 i k)
                           (aref m2 k j))))))
    result))

(defun apply-transform (mat vec)
  ;; Homogeneous coordinates are implicit. Every 3d vector
  ;; implicitly has a 4th coordinate with value 1. So we have
  ;; a 4x4 matrix times a 4d column vector.
  ;;  [m1  m2  m3  m4   [x
  ;;   m5  m6  m7  m8    y
  ;;   m9  m10 m11 m12   z
  ;;   m13 m14 m15 m16]  1]
  ;; After the multiplication, rescale so that the implicit 4th
  ;; coordinate is still 1.
  (flet ((dim (i)
           (+ (* (aref mat i 0) (vx vec))
              (* (aref mat i 1) (vy vec))
              (* (aref mat i 2) (vz vec))
              (aref mat i 3))))
    (let ((w (dim 3)))
      (when (zerop w)
        ;; Not sure what the effect of this will be, or
        ;; if w can realistically become 0? Maybe that's
        ;; what gimble lock is? But anyway, doing this just
        ;; to avoid a crash.
        (setf w 1))
      (vec3 (/ (dim 0) w)
            (/ (dim 1) w)
            (/ (dim 2) w)))))

(defun translation-transform (vec)
  (matrix 1 0 0 (vx vec)
          0 1 0 (vy vec)
          0 0 1 (vz vec)
          0 0 0 1))

(defun scale-transform (vec)
  (matrix (vx vec) 0 0 0
          0 (vy vec) 0 0
          0 0 (vz vec) 0
          0 0 0 1))

(defun reorient-transform (source-vec target-vec)
  ;; Take the cross product of the source & target, and use
  ;; it as an axis of rotation.
  (let ((cp (cross-product source-vec target-vec)))
    (if (zero-vector? cp)
        ;; Cross product is zero, can't be used as axis.
        ;; Vectors must be pointing in opposite directions, so
        ;; this works instead.
        (scale-transform (vec3 -1 -1 -1))
        (let ((n-source (v-normalize source-vec))
              (n-target (v-normalise target-vec)))
          (rotate-around-transform
           (v-normalise! (cross-product n-source n-target))
           (acos (v-dot n-source n-target)))))))

(defun rotate-around-transform (v angle)
  ;; TODO
  )
