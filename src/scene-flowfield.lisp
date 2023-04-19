;; References:
;;   Coding Train, https://www.youtube.com/watch?v=BjoM9oKOAKY
;;     (there are some useful comments such as how to
;;      make the particles more liquid-like and how to optimise)
;;   Tyler Hobbs article, https://tylerxhobbs.com/essays/2020/flow-fields
;;   Steve's Makerspace, https://www.youtube.com/watch?v=oKwi8h_yTsY&list=LL&index=20&t=2s

(in-package sketches)

(defsketch scene-flow
    ((width 800)
     (height 600)
     (copy-pixels t)
     (rng (random-state:make-generator 'random-state:mersenne-twister-64))
     (particles (loop repeat 100
                      collect (make-particle (random-state:random-int rng 0 width)
                                             (random-state:random-int rng 0 height))))
     (colour (rgb 0 0 0 0.1))
     (pen (make-pen :stroke colour :fill colour :weight 1))
     (flowfield (make-flowfield (make-vnoise) :spacing 50 :strength 0.5))
     (max-velocity 5))
  (with-pen pen
    (loop for particle in particles
          do (apply #'line (concatenate 'list (pos particle) (prev-position particle)))
          do (let ((dv (flowfield-get-effect flowfield (pos particle))))
               (update-particle-state! particle dv max-velocity 0 width 0 height)))))

(defmethod setup ((instance scene-flow) &key &allow-other-keys)
  (background +white+))
