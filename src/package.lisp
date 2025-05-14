(defpackage :sketches
  (:use :cl :sketch)
  ;; These exports are used in the sub-packages in which
  ;; sketches are defined.
  (:export
   :def-sketch-package
   :sketch-pkg-suffix
   :load-static-resource
   :get-last-sketch

   :lerp
   :smoothstep
   :remap
   :halve
   :square
   :scalef
   :with-centered
   :round-to-nearest-multiple
   :outside-range-p

   :particle
   :pos
   :prev-position
   :velocity
   :make-particle
   :update-particle-state!

   :flowfield
   :make-flowfield
   :flowfield-get-effect
   :flowfield-inc-time!

   :vec
   :zeros-vec
   :vec2
   :v-copy
   :vx
   :vy
   :vz
   :vn
   :vr
   :v-theta
   :v-copy
   :v=
   :v+
   :v+!
   :v-
   :v-!
   :v*!
   :v/!
   :v-scale
   :v-scale!
   :v-length
   :v-normalise
   :v-normalise!
   :v-rescale
   :v-rescale!
   :v-clamp
   :v-clamp!
   :get-nearest-point-on-grid
   :get-pos-on-unit-circle
   :v-dot
   :v-rotate
   :perpendicular-clockwise
   :perpendicular-anticlockwise
   :v->polar!
   :v->polar
   :polar-vec
   :euclidean-distance

   :palette-name
   :palette-colours
   :make-palette
   :get-palette
   :next-colour
   :reset-palette
   :shuffle-palette
   :palette-num-colours
   :random-palette
   :list-palettes
   :num-colours
   :nth-colour

   :def-lsystem
   :evaluate-lsystem

   :make-world
   :enable-gravity
   :disable-gravity
   :add-particle
   :with-particle-xy
   :update-world
   :add-bounds
   ))
