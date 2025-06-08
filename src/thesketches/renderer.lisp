(sketches:def-sketch-package renderer)
(in-package kg.sketch.renderer)

;;;; Triangle renderer and accompanying camera model.
;;;; 
;;;; Mostly based on this guide, especially the "Virtual Pinhole Camera" chapter.
;;;;   https://www.scratchapixel.com/lessons/3d-basic-rendering/3d-viewing-pinhole-camera/how-pinhole-camera-works-part-1.html

;; TODO (matrix math)
;;  --> double-check the acos(dot(v1,v2)) math
;;      I guess it works in sb-cga, so should work here?
;;  --> copy impl. of rotate-around-transform from sb-cga

(defclass camera ()
  ((pos
    :initarg :pos
    :initform (vec3 0 0 0)
    :accessor pos)
   (dir
    :initarg :dir
    :initform (vec3 0 0 1)
    :accessor dir)
   (focal-length
    :initarg :focal-length
    :accessor focal-length)
   (x-bound
    :initarg :x-bound
    :accessor x-bound)
   (y-bound
    :initarg :y-bound
    :accessor y-bound)
   (transform-mat
    :accessor transform-mat)))

(defmethod initialize-instance :after ((cam camera) &key &allow-other-keys)
  (setf (transform-mat cam)
        ;; Camera is at position P and points in direction D.
        ;; First, transform the coordinate system so that the camera
        ;; is at the origin (translation). Then rotate so that the camera
        ;; is pointing along the positive z-axis.
        ;; Matrix multiplication is how we compose these operations.
        (matrix*
         (translation-transform (v-scale -1 (pos cam)))
         (reorient-transform (dir cam) (vec3 0 0 1)))))

(defsketch renderer
    ((width 400)
     (height 400)
     (vertices
      (list (vec3 -1 -1 0)
            (vec3 0 1 0)
            (vec3 1 -1 0)))
     (cam (make-instance 'camera
                         :focal-length 1
                         :x-bound 10
                         :y-bound 10
                         :pos (vec3 0 0 -1)
                         :dir (vec3 0 0 1)))
     (y-axis :up))
  ;; TODO:
  ;; 0. Swap out sb-cga for a library that accepts non-single-float
  ;;    types in vectors.
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
             ;; The goal is to map the vertex onto a 2d "canvas" in front
             ;; of the camera. The canvas is like the retina of the eye, or
             ;; the photoreceptors (?) in a camera. It shows the camera's 2d view
             ;; of the 3d world.
             ;; First, transform coordinate system so that the
             ;; camera is at origin and facing in positive z direction.
             (let* ((new-vert
                      (apply-transform vert (transform-mat cam)))
                    ;; Then project point onto canvas. 
                    ;; For a given vertex (x,y,z) and focal length f (which is
                    ;; the camera's distance from the canvas), the projected x
                    ;; coordinate is given by:
                    ;;    f/z = x'/x,
                    ;;    x' = x(f/z).
                    ;; Similarly, the projected y coordinate is:
                    ;;    y' = y(f/z).
                    (px (* (vx new-vert) f (/ (vz new-vert))))
                    (py (* (vy new-vert) f (/ (vz new-vert)))))
               ;; Then blow up to size of screen.
               (values
                (* width (/ (+ px x-bound) (* 2 x-bound)))
                (* height (/ (+ py y-bound) (* 2 y-bound)))))))
      (destructuring-bind (v1 v2 v3) vertices
        ;; Map all 3 vertices to the screen. When we draw a triangle
        ;; consisting of those mapped vertices, it'll appear as it would
        ;; to the camera.
        (multiple-value-bind (x1 y1) (project-pt v1)
          (multiple-value-bind (x2 y2) (project-pt v2)
            (multiple-value-bind (x3 y3) (project-pt v3)
              (with-pen (:fill colour)
                (polygon x1 y1 x2 y2 x3 y3)))))))))
