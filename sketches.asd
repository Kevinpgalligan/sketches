(defpackage :sketches-asd
  (:use :cl :asdf))

(in-package :sketches-asd)

(defsystem sketches
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:sketch :random-state :alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "tools")
               (:file "tools-particle")
               (:file "tools-flowfield")
               (:file "vec2")
               (:file "noise")
               (:file "stars")
               (:file "rain")
               (:file "heightmap")
               (:file "unknown-pleasures")
               (:file "snow")
               (:file "scene-flowfield")
               (:file "dots")))
