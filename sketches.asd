(defpackage :sketches-asd
  (:use :cl :asdf))

(in-package :sketches-asd)

(defsystem sketches
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:sketch :random-state :alexandria :closer-mop :str)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "packaging")
               (:file "tools")
               (:file "particle")
               (:file "flowfield")
               (:file "vec")
               (:file "noise")
               (:file "palette")
               (:file "resources")
               (:file "lsystem")
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
                 ))))
