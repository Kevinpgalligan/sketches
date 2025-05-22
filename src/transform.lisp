(in-package sketches)

;; A shim over the sb-cga library, for 3d linear algebra.
;; Since sb-cga operates on float arrays, input vectors should
;; be created with vec3f.
;; Only the bare minimum is shimmed in order to implement my
;; triangle renderer.

(defun matrix* (&rest args)
  (apply #'sb-cga:matrix* args))

(defun apply-transform (vec mat)
  (sb-cga:transform-point vec mat))

(defun reorient-transform (source-vec target-vec)
  (if (zerop (v-length (sb-cga:cross-product source-vec target-vec)))
      ;; Workaround for what I think is a bug in sb-cga.
      ;; Internally, it takes the cross product of the two
      ;; vectors, then uses that as an anchor for the rotation.
      ;; But if the vectors point in opposite directions, the
      ;; cross product is all-zero, and attempting to use this
      ;; as the anchor gives a result matrix full of NaNs.
      (sb-cga:scale* -1.0 -1.0 -1.0)
      (sb-cga:reorient source-vec target-vec)))

(defun translation-transform (vec)
  (sb-cga:translate vec))
