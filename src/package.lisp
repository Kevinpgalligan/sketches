(defpackage :sketches
  (:use :cl :sketch)
  ;; These exports are used in the sub-packages in which
  ;; sketches are defined.
  (:export
   :def-sketch-package
   :sketch-pkg-suffix
   :load-static-resource

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
   :v-copy
   :v=
   :v+
   :v-
   :v*
   :v/
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

   :make-perlin-noise
   :make-vnoise
   :noise-get

   :get-palette
   :next-colour
   :reset-palette
   :shuffle-palette
   :palette-num-colours
   ))
