(sketches:def-sketch-package renderer)
(in-package kg.sketch.renderer)

;;;; Triangle renderer and accompanying camera model.
;;;; 
;;;; Mostly based on this, especially the "Virtual Pinhole Camera" chapter.
;;;;   https://www.scratchapixel.com/lessons/3d-basic-rendering/3d-viewing-pinhole-camera/how-pinhole-camera-works-part-1.html

(defclass camera ()
  ((focal-length
    :initarg :focal-length
    :accessor focal-length)
   (x-bound
    :initarg :x-bound
    :accessor x-bound)
   (y-bound
    :initarg :y-bound
    :accessor y-bound)))

(defsketch raster
    ((width 400)
     (height 400)
     (vertices
      (list (vec3 -4 2 2)
            (vec3 0 5 2)
            (vec3 2 -5 2)))
     (cam (make-instance 'camera
                         :focal-length 1
                         :x-bound 10
                         :y-bound 10))
     (y-axis :up))
  ;; TODO:
  ;; 0. Camera position and direction.
  ;;    (sb-cga to to transformations?)
  ;; 1. Frustum culling, z ordering of triangles, and also filter
  ;;    triangles facing the wrong way.
  ;; 2. Load model (bunch of triangles & their normals).
  (draw-triangle cam vertices +red+ width height)
  (stop-loop))

(defun draw-triangle (cam vertices colour width height)
  (with-accessors ((f focal-length)
                   (x-bound x-bound)
                   (y-bound y-bound))
      cam
    (flet ((project-pt (vert)
             ;; First project onto canvas.
             (let ((px (* (vx vert) f (/ (vz vert))))
                   (py (* (vy vert) f (/ (vz vert)))))
               ;; Then blow up to size of screen.
               (values
                (* width (/ (+ px x-bound) (* 2 x-bound)))
                (* height (/ (+ py y-bound) (* 2 y-bound)))))))
      (destructuring-bind (v1 v2 v3) vertices
        ;; Project all 3 vertices onto screen. Camera is assumed to be
        ;; at origin and pointing in direction of positive z-axis, so
        ;; there's no messing about with transformations into camera space.
        ;; For a given vertex (x,y,z), focal length f, projected x coordinate is
        ;; given by:
        ;;    f/z = x'/x,
        ;;    x' = x(f/z).
        ;; The projected y coordinate is:
        ;;    y' = y(f/z).
        (multiple-value-bind (x1 y1) (project-pt v1)
          (multiple-value-bind (x2 y2) (project-pt v2)
            (multiple-value-bind (x3 y3) (project-pt v3)
              (with-pen (:fill colour)
                (polygon x1 y1 x2 y2 x3 y3)))))))))
