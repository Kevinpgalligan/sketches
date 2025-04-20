(in-package sketches)

(defparameter *colours-by-palette* (make-hash-table))

(defun add-palette (name specs)
  (setf (gethash name *colours-by-palette*)
        (mapcar #'parse-colour specs)))

(defun parse-colour (spec)
  (if (stringp spec)
      (hex-to-color spec)
      (apply #'rgb-255 spec)))

(defparameter *raw-palettes*
  '((:sadiq ("#897C84" "#365E31" "#626A5E" "#AE5F44" "#3BA061" "#579B69"
             "#CBB058" "#4C7594" "#5FBDCA" "#36B3C9" "#9EADAE" "#D7C9AB"
             "#FBCCB9" "#F1DDD8"))
    (:palestine ("#E4312b" "#000000" "#FFFFFF" "#149954"))
    ;; From colorhunt.co! Made up the names myself.
    (:ch-greens ("#123524" "#3E7B27" "#85A947" "#EFE3C2"))
    (:ch-lolipop ("#5CB338" "#ECE852" "#FFC145" "#FB4141"))
    (:ch-suntan ("#FEF3E2" "#FAB12F" "#FA812F" "#FA4032"))
    (:ch-skeleton ("#E4E0E1" "#D6C0B3" "#AB886D" "#493628"))
    (:ch-pinkice ("#FF8282" "#FF6363" "#BEE4D0" "#DBFFCB"))
    (:ch-blues ("#213448" "#547792" "#94B4C1" "#ECEFCA"))
    (:ch-bloodice ("#D84040" "#A31D1D" "#ECDCBF" "#F8F2DE"))
    (:ch-beach ("#FFA725" "#FFF5E4" "#C1D8C3" "#6A9C89"))
    (:ch-arcticsailor ("#F1EFEC" "#D4C9BE" "#123458" "#030303"))
    (:ch-bluegreen ("#328E6E" "#67AE6E" "#90C67C" "#E1EEBC"))))

(loop for raw-palette in *raw-palettes*
      do (apply #'add-palette raw-palette))

(defclass palette ()
  ((name :initarg :name :accessor palette-name)
   (colours :initarg :colours :accessor palette-colours)
   (num-colours :initarg :num-colours)
   (colour-index :initform 0)))

(defun make-palette (name colours)
  (make-instance 'palette :name name
                          :colours colours
                          :num-colours (length colours)))

(defun get-palette (name)
  "Fetches a new palette."
  (let ((colour-list (gethash name *colours-by-palette*)))
    (if colour-list
        (make-palette name (copy-list colour-list))
        (error (format nil "Palette '~a' not found." name)))))

(defun next-colour (palette)
  "Gets next colour from a palette. Loops back to the start if all
the colours have been exhausted."
  (with-slots (colours colour-index num-colours) palette
    (let ((c (elt colours colour-index)))
      (incf colour-index)
      (when (>= colour-index num-colours)
        (setf colour-index 0))
      c)))

(defun reset-palette (palette)
  "Goes back to the first colour in the palette."
  (setf (slot-value palette 'colour-index) 0))

(defun shuffle-palette (palette)
  "Shuffles the ordering of colours in a palette."
  (alexandria:shuffle (slot-value palette 'colours))
  palette)

(defun palette-num-colours (palette)
  "Returns number of colours in a palette."
  (slot-value palette 'num-colours))

(defun random-palette ()
  (get-palette (first (alexandria:random-elt *raw-palettes*))))
