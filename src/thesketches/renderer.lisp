(sketches:def-sketch-package renderer)
(in-package kg.sketch.renderer)

;;;; Triangle renderer and accompanying camera model.
;;;; 
;;;; Mostly based on this guide, especially the "Virtual Pinhole Camera" chapter.
;;;;   https://www.scratchapixel.com/lessons/3d-basic-rendering/3d-viewing-pinhole-camera/how-pinhole-camera-works-part-1.html

;;;; TODO: Load a 3d model in .obj format

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
  (%update-camera-transform cam))

(defun %update-camera-transform (cam)
  (setf (transform-mat cam)
        ;; Camera is at position P and points in direction D.
        ;; First, transform the coordinate system so that the camera
        ;; is at the origin (translation). Then rotate so that the camera
        ;; is pointing along the positive z-axis.
        ;; Matrix multiplication is how we compose these operations.
        (matrix*
         (reorient-transform (dir cam) (vec3 0 0 1))
         (translation-transform (v-scale -1 (pos cam))))))

(defmethod (setf dir) :after (value (cam camera))
  (%update-camera-transform cam))

(defmethod (setf pos) :after (value (cam camera))
  (%update-camera-transform cam))

(defclass triangle ()
  ((v1 :initarg :v1 :accessor v1)
   (v2 :initarg :v2 :accessor v2)
   (v3 :initarg :v3 :accessor v3)
   (normal :initarg :normal :accessor normal)))

(defun make-triangle (v1 v2 v3 &key counter-clockwise)
  "The face of the triangle is always assumed to be on the side where
the vertices are in clockwise order. If COUNTER-CLOCKWISE, then the points
are assumed to be in counter-clockwise order, which has the effect of flipping
the face (and the normal vector)."
  (make-instance 'triangle
                 :v1 v1
                 :v2 v2
                 :v3 v3
                 :normal (v-scale! (if counter-clockwise -1 1)
                                   (v-normalise!
                                    (cross-product (v- v3 v2) (v- v1 v2))))))

(defsketch renderer
    ((width 400)
     (height 400)
     (triangles
      (list
       (make-triangle (vec3 -1 -1 0)
                      (vec3 0 1 0)
                      (vec3 1 -1 0))))
     (cam (make-instance 'camera
                         :focal-length 1
                         :x-bound 10
                         :y-bound 10
                         :pos (vec3 0 0 -1)
                         :dir (vec3 0 0 1)))
     (y-axis :up)
     (theta 0))
  (let* ((tri (first triangles))
         (center (v-scale (/ 3) (v+ (v1 tri) (v2 tri) (v3 tri))))
         (pos (v+ center (vec3 (cos theta) 0 (sin theta)))))
    (setf (pos cam) pos
          (dir cam) (v- center pos)))
  (draw-triangles cam triangles width height)
  (incf theta 0.01))

(defun draw-triangles (cam triangles width height)
  ;; The goal is to project each triangle onto a 2d "canvas" in front
  ;; of the camera. The canvas is like the retina of the eye, or
  ;; the photoreceptors in a camera. It shows the camera's 2d view
  ;; of the 3d world.
  (let ((to-draw (make-array 0 :fill-pointer t :adjustable t)))
    (loop for tri in triangles
          for dot = (v-dot (dir cam) (normal tri))
          ;; Filter out triangles that aren't facing the camera.
          when (> 0 dot)
            ;; Transform the remaining triangles into camera space.
            do (let ((verts (transform-triangle cam tri)))
                 ;; If all transformed vertices are behind the "canvas", then
                 ;; the triangle won't show up, so ignore it.
                 (when (loop for vert in verts
                               thereis (>= (vz vert) (focal-length cam)))
                   (vector-push-extend (list :transformed-verts verts
                                             :max-z (reduce #'max verts :key #'vz)
                                             :dot (abs dot))
                                       to-draw))))
    ;; Sort so that closer triangles are drawn last.
    (sort to-draw #'> :key (lambda (tri) (getf tri :max-z)))
    (loop for tri across to-draw
          do (draw-triangle cam
                            width
                            height
                            (getf tri :transformed-verts)
                            (getf tri :dot)))))

(defun transform-triangle (cam tri)
  (mapcar (lambda (vert)
            (apply-transform (transform-mat cam) vert))
          (list (v1 tri)
                (v2 tri)
                (v3 tri))))

(defun draw-triangle (cam width height verts dot)
  (destructuring-bind (v1 v2 v3)
      verts
    (with-accessors ((f focal-length)
                     (x-bound x-bound)
                     (y-bound y-bound))
        cam
      (flet ((project (vert)
               ;; For a given vertex (x,y,z) and focal length f (which is
               ;; the camera's distance from the canvas), the projected x
               ;; coordinate is given by:
               ;;    f/z = x'/x,
               ;;    x' = x(f/z).
               ;; Similarly, the projected y coordinate is:
               ;;    y' = y(f/z).
               (let ((px (* (vx vert) (/ f (vz vert))))
                     (py (* (vy vert) (/ f (vz vert)))))
                 ;; Then blow up to size of screen.
                 ;; The x-coordinate of the left side of the canvas is
                 ;;    (- x-bound)
                 ;; while the right side is
                 ;;    x-bound
                 ;; So, we want the point's offset from the left side to
                 ;; get its x-coordinate on the actual screen. Then divide
                 ;; by total width of canvas to get its position from 0-1.
                 ;; Scale up by the size of the actual screen and voila.
                 ;; Same for y-coordinate.
                 (values
                  (* width (/ (+ px x-bound) (* 2 x-bound)))
                  (* height (/ (+ py y-bound) (* 2 y-bound)))))))
        ;; Map all 3 vertices to the screen. When we draw a triangle
        ;; consisting of those mapped vertices, it'll appear as it would
        ;; to the camera.
        (multiple-value-bind (x1 y1) (project v1)
          (multiple-value-bind (x2 y2) (project v2)
            (multiple-value-bind (x3 y3) (project v3)
              (with-pen (:fill (gray (* 0.9 dot)))
                (polygon x1 y1 x2 y2 x3 y3)))))))))
