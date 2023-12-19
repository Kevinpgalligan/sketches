(sketches:def-sketch-package palette)
(in-package kg.sketch.palette)

;;; This sketch can be used to display palettes and generate
;;; new ones. Press spacebar to load/generate a new palette. Modify
;;; the slots of the sketch instance to specify the palette or palette
;;; generation technique.

(defsketch palette
    ((width 600)
     (height 400)
     (n-colours 5)
     (method :random)
     (base-colours-method :random) ; or :hash
     (name :sadiq))
  (draw-colours
   (case method
     ;; Entirely random colours.
     (:random (loop repeat n-colours
                    collect (random-color)))
     ;; Use HASH-COLOR.
     (:hash (loop repeat n-colours
                  for i = (random 100000) then (1+ i)
                  collect (hash-color i)))
     ;; "Rotate" an initial set of colours. (Swapping R, G & B values).
     (:rotate (let ((base-cols (gen-base-colours (ceiling n-colours 2)
                                                 base-colours-method)))
                (append base-cols
                        (loop for c in base-cols
                              collect (color-filter-rotate c))
                        (loop for c in base-cols
                              collect (color-filter-rotate
                                       (color-filter-rotate c))))))
     ;; Invert an initial set of colours.
     (:invert (let ((base-cols (gen-base-colours (ceiling n-colours 2)
                                                 base-colours-method)))
                (append base-cols
                        (loop for c in base-cols
                              collect (color-filter-invert c)))))
     ;; Load an existing palette.
     (:named (let ((pal (get-palette name)))
               (loop repeat (palette-num-colours pal)
                     collect (next-colour pal)))))
   0
   height
   width)
  (stop-loop))

(defun draw-colours (colours y-offset palette-height width)
  (let* ((num-colours (length colours))
         (width-per-col (/ width num-colours)))
    (loop for col in colours
          for i = 0 then (1+ i)
          do (with-pen (make-pen :fill col)
               (rect (* i width-per-col) y-offset width-per-col palette-height)))))

(defun gen-base-colours (n-colours method)
  (case method
    (:random
     (loop repeat n-colours
           collect (random-color)))
    (:hash
     (loop repeat n-colours
           for i = (random 100000) then (1+ i)
           collect (hash-color i)))))

(defmethod on-key ((instance palette) key state)
  (when (and (eq key :space) (eq state :keyup))
    (start-loop)))
