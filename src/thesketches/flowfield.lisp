;; References:
;;   Coding Train, https://www.youtube.com/watch?v=BjoM9oKOAKY
;;     (there are some useful comments such as how to
;;      make the particles more liquid-like and how to optimise)
;;   Tyler Hobbs article, https://tylerxhobbs.com/essays/2020/flow-fields
;;   Steve's Makerspace, https://www.youtube.com/watch?v=oKwi8h_yTsY&list=LL&index=20&t=2s

(sketches:def-sketch-package flowfield)
(in-package kg.sketch.flowfield)

(defsketch flowfield-test
    ((width 800)
     (height 600)
     (copy-pixels t)
     (rng (random-state:make-generator 'random-state:mersenne-twister-64))
     (particles (loop repeat 10
                      collect (make-particle (random-state:random-int rng 0 width)
                                             (random-state:random-int rng 0 height))))
     (colour (rgb 0 0 0 0.1))
     (pen (make-pen :stroke colour :fill colour :weight 1))
     (flowfield (make-flowfield :spacing 50 :strength 0.07))
     (max-velocity 5))
  (with-pen pen
    (loop for particle in particles
          do (apply #'line (concatenate 'list (pos particle) (prev-position particle)))
          do (let ((dv (flowfield-get-effect flowfield (pos particle))))
               (update-particle-state! particle dv max-velocity 0 width 0 height
                                       :wrap-around-p t))))
  ;; For debugging.
  ;(draw flowfield :width width :height height)
  )

(defmethod setup ((instance flowfield-test) &key &allow-other-keys)
  (background +white+))
