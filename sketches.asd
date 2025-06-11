(defpackage :sketches-asd
  (:use :cl :asdf))

(in-package :sketches-asd)

(defsystem sketches
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:sketch :random-state :alexandria :closer-mop :str :noisy :ltk)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "packaging")
               (:file "tools")
               (:file "particle")
               (:file "flowfield")
               (:file "vec")
               (:file "palette")
               (:file "resources")
               (:file "lsystem")
               (:file "widgets")
               (:file "physics")
               (:file "transform")
               (:module "thesketches"
                :serial t
                :components
                ((:file "stars")
                 (:file "rain")
                 (:file "heightmap")
                 (:file "unknown-pleasures")
                 (:file "snow")
                 (:file "flowfield")
                 (:file "dots")
                 (:file "growth")
                 (:file "groove2")
                 (:file "mandelbrot")
                 (:file "squarez")
                 (:file "palette")
                 (:file "xmas")
                 (:file "invaders")
                 (:file "lorenz")
                 (:file "toast")
                 (:file "reaction-diffusion")
                 (:file "trees")
                 (:file "swirl")
                 (:file "reuleaux")
                 (:file "lsystems")
                 (:file "terrain")
                 (:file "nested-squares")
                 (:file "dandy")
                 (:file "boids")
                 (:file "nadasurf")
                 (:file "barricelli")
                 (:file "physics-test")
                 (:file "keffiyeh")
                 (:file "renderer")
                 ))))
