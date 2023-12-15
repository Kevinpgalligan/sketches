(in-package sketches)

(defparameter *noise-size* 256)
(defparameter *permutation-table*
  (make-array (list *noise-size*)
              :initial-contents
              '(151 160 137 91 90 15  
                131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23  
                190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33  
                88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166  
                77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244  
                102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196  
                135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123  
                5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42  
                223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9  
                129 22 39 253 9 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228  
                251 34 242 193 238 210 144 12 191 179 162 241  81 51 145 235 249 14 239 107  
                49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254  
                138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180)))
(defparameter *noisegen* (random-state:make-generator 'random-state:mersenne-twister-64))
(defparameter *offsets-table* (make-hash-table))

(defun permute (i)
  (aref *permutation-table* (mod i *noise-size*)))

(defun make-perlin-noise (dimensions &key seed)
  (when (null seed)
    (setf seed (1+ (random 1000000))))
  (let* ((r (make-array (list *noise-size*)))
         ;; We will reuse these so that we don't need to allocate
         ;; memory / compute stuff every time the noise function is called.
         (base-corner (loop repeat dimensions collect nil))
         (offsets (generate-corner-offsets dimensions))
         (num-corners (expt 2 dimensions))
         (vals (loop repeat num-corners collect nil)))
    ;; Create gradient vectors.
    (let ((rng (random-state:make-generator 'random-state:mersenne-twister-64)))
      (random-state:reseed rng seed)
      (dotimes (i *noise-size*)
        (setf (aref r i)
              (if (= dimensions 1)
                  ;; Edge case, in 1 dimension you use a scalar between -1 and 1
                  ;; instead of a direction vector, since direction doesn't mean
                  ;; much in 1 dimension.
                  (list (random-state:random-float rng -1 1))
                  (generate-point-on-unit-sphere dimensions rng)))))
    ;; This is the interface to creating noise: a function that takes a
    ;; point and returns a noise value.
    (lambda (&rest point)
      ;; Set the base corner in the grid.
      (overwrite-list! base-corner point #'floor)
      ;; Compute the dot product between the gradient at each corner surrounding the point, and
      ;; the vector between the given corner and the point.
      (loop for offset in offsets
            for gradient = (fetch-gradient r base-corner offset)
            for vals-cell on vals
            do (rplaca vals-cell
                       (if (= dimensions 1)
                           ;; Edge case again!
                           (let ((x (car point))
                                 (x1 (+ (car base-corner) (car offset)))
                                 (m (car gradient)))
                             (* m (- x x1)))
                           (dot-gradient-with-direction gradient base-corner offset point))))
      ;; Now interpolate between the values.
      (loop for x in point
            for f = (multiple-value-bind (i fractional-part)
                        (floor x)
                      (declare (ignore i))
                      fractional-part)
            for remaining-values = num-corners then (halve remaining-values)
            while (> remaining-values 1)
            do (loop for (v1 . (v2 . rest)) on vals by #'cddr
                     for vals-cell on vals
                     for i = 0 then (+ 2 i)
                     while (< i remaining-values)
                     do (rplaca vals-cell (lerp v1 v2 (smoothstep f)))))
      (remap (car vals) -1 1 0 1))))

(defun overwrite-list! (xs ys f)
  "Takes 2 lists XS and YS, of the same length. Overwrites the
values of XS by calling the function F on each of the values of YS."
  (loop for x-cell on xs
        for y in ys
        do (rplaca x-cell (funcall f y))))

(defun fetch-gradient (r base-corner offset)
  (aref r (get-gradient-index base-corner offset)))

(defun get-gradient-index (base-corner offset)
  (let ((i 0))
    (loop for b in base-corner
          for o in offset
          ;; Adding b & o gives a coordinate of the corner.
          do (setf i (permute (+ i b o))))
    i))

(defun dot-gradient-with-direction (gradient base-corner offset point)
  ;; Take the vector that points from the corner (given by adding base-corner & offset) to
  ;; the point, and dot it with the gradient.
  (loop for g in gradient
        for x in point
        for b in base-corner
        for o in offset
        sum (* g (- x (+ b o)))))

(defun generate-point-on-unit-sphere (dimensions rng)
  ;; Rejection sampling: generate random points with coordinates in the
  ;; range [-1, 1]. Keep rejecting until one of the points falls within the
  ;; unit sphere. In higher dimensions this will take a long time because the
  ;; volume of the sphere is relatively small compared to the cube/box. Alternatively,
  ;; you can sample coordinates from the Gaussian distribution and the resulting
  ;; point will be uniformly distributed over the sphere. For our target dimensions,
  ;; this should be fine (~3 attempts on average to generate a point in 4 dimensions).
  (let ((point (loop repeat dimensions collect nil))
        (length 0))
    (loop do (loop for coord on point
                   do (setf (car coord) (random-state:random-float rng -1 1)))
          do (setf length (list-euclidean-length point))
          when (<= length 1)
            return (list-normalise! point length))))

(defun list-euclidean-length (list)
  (sqrt (loop for x in list sum (square x))))

(defun list-normalise! (list length)
  (loop for remaining on list
        do (scalef (car remaining) (/ 1 length)))
  list)

(defun make-vnoise (&key (seed 7))
  (let* ((r (make-array (list *noise-size*)))
         (get-value
           (lambda (coords)
             ;; Here we map a multidimensional point into the 1-dimensional
             ;; array of values using the permutation table.
             (aref r
                   (reduce (lambda (x y)
                             (funcall #'permute (+ x y)))
                           coords
                           :initial-value 0)))))
    (random-state:reseed *noisegen* seed)
    (dotimes (i *noise-size*)
      (setf (aref r i) (random-state:random-unit *noisegen*)))
    (lambda (&rest coords)
      (let* ((offsets (get-corner-offsets (length coords)))
             (base-corner (mapcar #'floor coords))
             ;; Fractional parts of the coordinates, used to interpolate between
             ;; the values at each corner of the cell that this point is in.
             (fs (mapcar (lambda (x)
                           (multiple-value-bind (i f) (floor x)
                             (declare (ignore i))
                             f))
                         coords)))
        ;; These are the values at each corner of the cell, now we have to interpolate
        ;; between them. They've been generated in an order such that adjacent corners
        ;; are next to each other in the list, so we repeatedly interpolate between pairs until
        ;; only 1 value is left.
        (let ((vals
                (loop for offset in offsets
                      collect (funcall get-value (mapcar #'+ base-corner offset)))))
          (loop for f in fs
                do (combine-pairs! (lambda (v1 v2)
                                     (lerp v1 v2 (smoothstep f)))
                                   vals))
          (car vals))))))

(defun combine-pairs! (binary-function list)
  (loop for pair on list
        while pair
        do (progn
             (setf (car pair) (funcall binary-function (car pair) (cadr pair)))
             (setf (cdr pair) (cddr pair)))))

(defun noise-get (noise &rest coords)
  (apply noise coords))

(defun get-corner-offsets (num-dimensions)
  ;; Cache the offsets so that we don't need to generate
  ;; them for every single call.
  (or (gethash num-dimensions *offsets-table*)
      (let ((offsets (generate-corner-offsets num-dimensions)))
        (setf (gethash num-dimensions *offsets-table*) offsets)
        offsets)))

(defun generate-corner-offsets (n)
  (labels ((rec (n sequences)
             (if (<= n 0)
                 sequences
                 (rec (1- n)
                      (nconc (mapcar (lambda (sequence)
                                       (reverse (cons 0 (reverse sequence))))
                                     sequences)
                             (mapcar (lambda (sequence)
                                       (reverse (cons 1 (reverse sequence))))
                                     sequences))))))
    (rec n (list (list)))))
