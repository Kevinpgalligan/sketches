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

;; Could expand this one with other shape types, but I'm not sure if the juice
;; would be worth the squeeze.
(defsketch zeno-random
    ((width 400)
     (height 400)
     (pal (random-palette))
     (reset-on :space)
     (min-lines 12)
     (max-lines 20)
     (steps 12)
     (starting-radius 50)
     (f 0.8)
     (field (make-flowfield :strength 0.8)))
  (shuffle-palette pal)
  (background (next-colour pal))

  (loop repeat 2
        do (with-pen (make-pen :fill (next-colour pal))
             (let ((pt (vec2 (random (float width)) (random (float height))))
                   (bounds (vec2 width height)))
               (loop repeat steps
                     for r = starting-radius then (* f r)
                     do (progn
                          (let ((x (vx pt))
                                (y (vy pt)))
                            (circle x y r)
                            (when (< x r)
                              (circle (+ width x) y r))
                            (when (< (- width x) r)
                              (circle (- x width) y r))
                            (when (< y r)
                              (circle x (+ height y) r))
                            (when (< (- height y) r)
                              (circle x (- y height) r)))
                          (v%!
                           (v+! pt
                                (v-rescale! (+ r (* f r))
                                            (flowfield-get-effect field pt)))
                           bounds))))))

  (stop-loop))
