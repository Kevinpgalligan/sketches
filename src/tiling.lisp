(in-package sketches)

(defun draw-fixed-tiles (draw-fn width height num-tiles-across num-tiles-down &key (x 0) (y 0))
  "DRAW-FN should accept x position, y position, width and height of a given tile. There are no
safeguards to prevent it drawing outside its assigned area."
  (let ((tile-width (/ width num-tiles-across))
        (tile-height (/ height num-tiles-down)))
    (loop for i below num-tiles-across
          do (loop for j below num-tiles-down
                   do (funcall draw-fn (+ x (* i tile-width)) (+ y (* j tile-height)) tile-width tile-height)))))

(defun draw-random-tiles (draw-fn width height &key (x 0) (y 0) (split-chance 0.8) (min-depth 2) (max-depth 7))
  (labels ((recur (depth x y width height)
             (let ((split?
                     (cond ((< depth min-depth) t)
                           ((>= depth max-depth) nil)
                           (t (< (random 1.0) split-chance)))))
               (if split?
                   (let ((new-depth (1+ depth)))
                     (if (< width height)
                         (let ((half-height (/ height 2)))
                           (recur new-depth x y width half-height)
                           (recur new-depth x (+ y half-height) width half-height))
                         (let ((half-width (/ width 2)))
                           (recur new-depth x y half-width height)
                           (recur new-depth (+ x half-width) y half-width height))))
                   (funcall draw-fn x y width height)))))
    (recur 0 x y width height)))
