(sketches:def-sketch-package skyscrapers)
(in-package kg.sketch.skyscrapers)

;; This is a toy rasteriser for rendering skyscrapers.
;; Algo steps:
;;   1. Make scrapers out of triangles.
;;   2. Take triangles, filter out ones not facing camera.
;;   3. Sort by furthest away first, scan through 'em.
;;   4. For each triangle, project it onto the screen, colour based
;;      on angle w/ light.

;; Camera
;;   focus/whatever (position)
;;   direction
;;   depth
;;   height
;;           ... H
;;           ^
;;         /
;;       /|
;;     /  | h
;;   /    |  
;; /______|
;;     d
;; By similar triangles... h/d = H/D, h= (d/D) H
;; If h>height, drop it. Do we need to check if something is behind the camera?
;; Is that a faster way of filtering out a point?
;; That would be... if the normal OR the ray from point to focus is pointing
;; same way as camera direction. Okay, fine for normal... but how about the camera
;; position?
(defsketch skyscrapers
    ((width 500)
     (height 500)
     (light-pos (vec 10 250 -100))
     (camera-pos (vec 0 0 0))
     (camera-dir (vec 0 1 1))
     )
  )
