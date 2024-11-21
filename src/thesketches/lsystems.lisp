(sketches:def-sketch-package lsystems.koch)
(in-package kg.sketch.lsystems.koch)

(def-lsystem koch
    ((#\F "F+F-F-F+F")))

(defsketch koch
    ((depth 3)
     (width 300)
     (height 300)
     (y-axis :up)
     (line-length 10))
  (background +white+)
  (translate 10 100)
  (with-pen (make-pen :stroke +red+ :weight 2)
    (flet ((ev (c)
             (case c
               (#\F
                (line 0 0 line-length 0)
                (translate line-length 0))
               (#\+ (rotate 90))
               (#\- (rotate (- 90))))))
      (evaluate-lsystem 'koch #'ev :depth depth :axiom "F")))
  (stop-loop))

(sketches:def-sketch-package lsystems.sierspinski)
(in-package kg.sketch.lsystems.sierspinski)

(def-lsystem sierspinski
    ((#\F "F-G+F+G-F")
     (#\G "GG")))

(defsketch sierspinski
    ((depth 4)
     (y-axis :up)
     (angle 120)
     (line-length 20))
  (background +white+)
  (translate 10 10)
  (with-pen (make-pen :stroke +black+ :weight 2)
    (flet ((ev (c)
             (case c
               (#\F
                (line 0 0 line-length 0)
                (translate line-length 0))
               (#\G
                (line 0 0 line-length 0)
                (translate line-length 0))
               (#\+ (rotate (- angle)))
               (#\- (rotate angle)))))
      (evaluate-lsystem 'sierspinski #'ev :depth depth :axiom "F-G-G")))
  (stop-loop))
