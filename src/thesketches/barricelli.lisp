;;;; Inspired by pictures from this blog post:
;;;;    https://akkartik.name/post/2024-08-30-devlog
;;;; And vaguely following the 1987 paper by Nils Aall Barricelli
;;;; linked therein. There's a lot of room to add new rules, like
;;;; mutation, reproduction, etc. Also, I have in no way debugged this
;;;; and I'm not entirely sure it's correct, but the images look kinda cool?

;;;; ==== INSTRUCTIONS
;;;; Press <SPACE> to generate a new picture.
;;;; Parameters to tweak:
;;;;   SIZE and N-GENERATIONS determine width and height.
;;;;   MAG makes each cell/tile map to MAGxMAG pixels.
;;;;   POSSIBLE-VALS are the actual integer values assigned to the 1d array
;;;;      and that evolve over time according to Barricelli rules.

(sketches:def-sketch-package barricelli)
(in-package kg.sketch.barricelli)

(defun get-bar-palette (n)
  ;; This just rotates the palette (in the sense of swapping R, G and B values)
  ;; once or twice so that there are enough unique colours for all the values.
  ;; Functionality like this should really be baked into the palette interface...
  ;; Like, "only return palettes with at least N colours", or "extend this palette
  ;; so it has N colours".
  (let ((pal (random-palette)))
    (if (<= n (palette-num-colours pal))
        pal
        (let ((rots (ceiling (palette-num-colours pal) n))
              (cs (palette-colours pal)))
          (make-palette (palette-name pal)
                        (append cs
                                (loop for c in cs
                                      collect (color-filter-rotate c))
                                (if (>= rots 3)
                                    (loop for c in cs
                                          collect (color-filter-rotate
                                                   (color-filter-rotate c)))
                                    (list))))))))
(defsketch barricelli
    ((size 400)
     (n-generations 400)
     (mag 1)
     (width (* mag size))
     (height (* mag n-generations))
     (possible-vals '(0 2 3 5)))
  (let* ((pal (get-bar-palette (length possible-vals)))
         (cvs (make-canvas size n-generations))
         (n-vals (length possible-vals))
         (life (make-array size :initial-element 0))
         (backup (copy-seq life))
         (colour-map (make-hash-table)))
    ;; Pick random colours for values.
    (loop for i from 0
          for v in possible-vals
          do (setf (gethash v colour-map) (next-colour pal)))
    ;; Initialise life grid with random values.
    (dotimes (j size)
      (setf (aref life j)
            (* (nth (random 2) (list -1 1))
               (nth (random n-vals) possible-vals))))
    (dotimes (i n-generations)
      ;; Draw this row / generation.
      (dotimes (j size)
        ;; Take abs because there are negative values..
        (canvas-paint cvs (gethash (abs (aref life j)) colour-map) j i))
      ;; Evolve it!
      (flet ((fill-backup (idx val)
               (setf (aref backup idx)
                     (if (not (= (aref backup idx) 0))
                         ;; Not empty, collision, kill it.
                         -1
                         ;; It's free for occupation!
                         val))))
        (loop for x across life
              for j from 0
              do (when (< 0 x)
                   (let* ((new-j (mod (+ j x) size))
                          (y (aref life new-j)))
                     (fill-backup new-j x)
                     (when (not (zerop y))
                       ;; Landed on a square that used to be occupied, so
                       ;; also propagate back the other direction by the old
                       ;; value of that square.
                       (let ((new-new-j (mod (+ j (* (signum x)
                                                     (abs y)))
                                             size)))
                         (fill-backup new-new-j x))))))
        ;; Swap out the two arrays.
        (rotatef life backup)
        ;; Make the collision squares (with value -1) empty.
        (loop for x across life
              for j from 0
              do (when (= x -1)
                   (setf (aref life j) 0)))
        ;; And empty the backup for the next iteration.
        (loop for j from 0 below size
              do (setf (aref backup j) 0))))
    (draw cvs :width width :height height)
    (stop-loop)))

(defmethod on-key ((instance barricelli) (key (eql :space)) (state (eql :up)))
  (start-loop))
