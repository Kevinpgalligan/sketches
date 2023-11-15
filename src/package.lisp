(defpackage :sketches
  (:use :cl :sketch)
  ;; These exports are used in the sub-packages in which
  ;; sketches are defined.
  (:export
   :def-sketch-package

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

   :vec2
   :vx
   :vy
   :v+
   :v-
   :v-scale
   :v-length
   :v-normalise
   :v-rescale
   :v-clamp
   :get-nearest-point-on-grid
   :get-pos-on-unit-circle

   :make-perlin-noise
   :make-vnoise
   :noise-get
   ))
