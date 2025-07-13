(sketches:def-sketch-package zeno)
(in-package kg.sketch.zeno)

(defsketch zeno
    ((width 350)
     (height 500)
     (pal (random-palette))
     (circrad 50)
     (rectsize 100)
     (f 0.8)
     (iters 12)
     (w 1))
  (shuffle-palette pal)
  (background (next-colour pal))

  (translate (+ 10 circrad) 10)

  (with-current-matrix
    (translate 0 (+ 10 circrad))
    (with-pen (:fill (next-colour pal) :weight w :stroke +black+)
      (dotimes (i iters)
        (circle 0 0 (- (* circrad (expt f i)) w))
        (translate 0 (* circrad (+ (expt f i) (expt f (1+ i))))))))

  (translate (+ 10 circrad) 10)

  (with-current-matrix
    (with-pen (:fill (next-colour pal) :weight w :stroke +black+)
      (dotimes (i iters)
        (let ((curr (* rectsize (expt f i))))
          (polygon 0 0 curr 0 0 curr)
          (translate 0 curr)))))

  (translate (+ 10 rectsize) 0)

  (with-current-matrix
    (with-pen (:fill (next-colour pal) :weight w :stroke +black+)
      (dotimes (i iters)
        (let ((curr-size (* rectsize (expt f i))))
          (rect 0 0 (- curr-size w) (- curr-size w))
          (translate (if (evenp i)
                         (* curr-size (- 1 f))
                         0)
                     curr-size)))))
  
  (stop-loop))
