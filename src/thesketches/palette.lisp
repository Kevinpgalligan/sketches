(sketches:def-sketch-package palette)
(in-package kg.sketch.palette)

;;; For displaying all the colour palettes.

(defsketch palette
    ((width 600)
     (height 400)
     (strip-height 50)
     (n-at-a-time (floor height strip-height))
     (pals (list-palettes)))
  (let ((this-time (subseq pals 0 n-at-a-time)))
    (setf pals (append (subseq pals n-at-a-time) this-time))
    (loop for pal-id in this-time
          for i from 0
          do (let* ((pal (get-palette pal-id))
                    (n-colours (num-colours pal))
                    (cwidth (ceiling width n-colours)))
               (loop for j from 0 below n-colours
                     do (with-pen (:fill (next-colour pal))
                          (rect (* j cwidth)
                                (* i strip-height)
                                cwidth
                                strip-height)))
               (with-font (make-font :color
                                     (destructuring-bind (r g b)
                                         (color-rgb (nth-colour pal 0))
                                       (if (< (+ r g b)
                                              (+ (- 1 r) (- 1 g) (- 1 b)))
                                           +white+
                                           +black+)))
                 (text (symbol-name pal-id) 0 (* i strip-height))))))
  (stop-loop))

(defmethod on-key ((instance palette) key state)
  (when (and (eq key :space) (eq state :up))
    (start-loop)))
