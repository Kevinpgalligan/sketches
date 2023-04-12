(in-package sketches)

;;;; Inspired by this:
;;;; https://www.reddit.com/r/commandline/comments/12gq436/does_this_look_cool/
;;;; The last pixels seem to not be coloured in properly, for some reason.

(defclass plant ()
  ((border :initarg :border :accessor border)
   (colour :initarg :colour :accessor colour)))

(defun surrounding-points (x y width-cells height-cells cells)
  (remove-if (lambda (pt)
               (or (< (car pt) 0)
                   (< (cadr pt) 0)
                   (>= (car pt) width-cells)
                   (>= (cadr pt) height-cells)
                   (> (aref cells (car pt) (cadr pt)) 0)))
             (list (list (1- x) y)
                   (list (1+ x) y)
                   (list x (1- y))
                   (list x (1+ y)))))

(defun fill-point (plant i x y cell-size cells)
  (setf (aref cells x y) i)
  (with-pen (make-pen :fill (colour plant))
    (rect (* cell-size x) (* cell-size y) cell-size cell-size)))

(defsketch growth
    ((width 500)
     (height 500)
     (cell-size 5)
     (width-cells (/ width cell-size))
     (height-cells (/ height cell-size))
     (copy-pixels t)
     (num-plants 10)
     (first-iteration t)
     (cells (make-array (list width-cells height-cells) :initial-element 0))
     (plants nil))
  (loop for plant in plants
        for i = 1 then (1+ i)
        when (border plant)
          do (let ((point (nth (random (length (border plant))) (border plant))))
               (when first-iteration
                 (fill-point plant i (car point) (cadr point) cell-size cells))
               (loop for pt in (surrounding-points (car point) (cadr point) width-cells height-cells cells)
                     do (destructuring-bind (x y) pt
                          (fill-point plant i x y cell-size cells)
                          (push pt (border plant))))
               (setf (border plant) (remove point (border plant)))))
  (setf first-iteration nil))

(defmethod setup ((instance growth) &key &allow-other-keys)
  (background +black+)
  (with-slots ((cells cells)
               (cell-size cell-size)
               (plants plants)
               (num-plants num-plants)
               (width-cells width-cells)
               (height-cells height-cells))
      instance
    (let ((colour-offset (random 500)))
      (loop repeat num-plants
            for i = 1 then (1+ i)
            do (loop do (let ((x (random width-cells))
                              (y (random height-cells)))
                          (when (zerop (aref cells x y))
                            (setf (aref cells x y) i)
                            (let ((plant (make-instance 'plant :border (list (list x y)) :colour (hash-color (+ colour-offset i)))))
                              (push plant plants))
                            (return))))))))
