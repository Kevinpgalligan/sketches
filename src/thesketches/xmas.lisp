(sketches:def-sketch-package xmas)
(in-package kg.sketch.xmas)

(defparameter *width-height-ratio* 0.8)
(defparameter *section-shrink-coeff* 0.75)
(defparameter *offset-proportion* 0.65)

(defsketch xmas
    ((width 500)
     (height 500)
     (middle-x (/ width 2))
     (num-trees 10)
     (tree-size 2000)
     (size-scale 500)
     (pal (get-palette :palestine))
     (watermelon (load-static-resource "watermelon.png"))
     (melon-size 100)
     (min-size (/ tree-size num-trees 3))
     (trees
      (loop for i from num-trees downto 1
            collect (list :size-denom (* size-scale (/ i num-trees))
                          :colour (next-colour pal)))))
  (background +white+)
  (loop for tree in trees
        for s = (* tree-size (/ (getf tree :size-denom) size-scale))
        do (let ((w (* s *width-height-ratio*)))
             (draw-tree (/ width 2)
                        (- (/ height 2) (/ s 2))
                        w
                        s
                        (getf tree :colour)))
        do (decf (getf tree :size-denom)))
  (setf trees
        (remove-if (lambda (tree) (<= (getf tree :size-denom) 0))
                   trees))
  (when (< (length trees) num-trees)
    (push (list :size-denom size-scale :colour (next-colour pal))
          trees))
  (with-pen (make-pen :weight 0)
    (draw watermelon
          (- (/ width 2) (/ melon-size 2))
          (- (/ height 2) (/ melon-size 2))
          :width melon-size
          :height melon-size)))

(defun draw-tree (x y width height colour)
  "Draws a Christmas tree in a rectangle where the middle of the top edge of the 
rectangle is at coordinates (x,y)."
  ;; Total height H, height of first section h, shrink
  ;; coefficient c, offset proportion o.
  ;; H = h + (h*c - h*o) + (h*c^2 - h*c*o)
  ;; h = H / (1 + c - o + c^2 - c*o)
  (let* ((c *section-shrink-coeff*)
         (o *offset-proportion*)
         (h (/ height (+ 1 c (- o) (* c c) (- (* c o)))))
         (by (+ y height)))
    (with-pen (make-pen :fill colour)
      (loop repeat 3
            for bwidth = width then (* bwidth c)
            for bheight = h then (* bheight c)
            do (progn
                 (polygon (- x (/ bwidth 2)) by
                          (+ x (/ bwidth 2)) by
                          x (- by bheight))
                 (setf by (+ by (* o bheight) (- bheight))))))))
