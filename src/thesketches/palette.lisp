(sketches:def-sketch-package palette)
(in-package kg.sketch.palette)

;; Test palette generation techniques as part of a sketch:
;;  - hash
;;  - invert
;;  - rotate
;;  - lerping
;;  - random
;; And, of course, sample from other people's palettes.
;; And have an option in the sketch to sample through all palettes
;; with their names.

(defparameter *name* :test)

(defsketch palette
    ((width 600)
     (height 400))
  (let* ((pal (get-palette *name*))
         (width-per-col (/ width (palette-num-colours pal))))
    (dotimes (i (palette-num-colours pal))
      (with-pen (make-pen :fill (next-colour pal))
        (rect (* i width-per-col) 0 width-per-col height)))))
