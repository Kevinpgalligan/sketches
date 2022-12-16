(in-package sketches)

;; TODO: change smoothstep interface, I don't think it
;; should do the lerp for us.

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

(defun make-vnoise (&key (dimensions 1) (seed 7))
  (let* ((offsets (generate-gray-code-sequences dimensions))
         (r (make-array (list *noise-size*)))
         (permute
           (lambda (i)
             (aref *permutation-table* (mod i *noise-size*))))
         (get-value
           (lambda (coords)
             ;; Here we map a multidimensional point into the 1-dimensional
             ;; array of values using the permutation table.
             (aref r
                   (reduce (lambda (x y)
                             (funcall permute (+ x y)))
                           coords
                           :initial-value 0)))))
    (random-state:reseed *noisegen* seed)
    (dotimes (i *noise-size*)
      (setf (aref r i) (random-state:random-unit *noisegen*)))
    (lambda (&rest coords)
      (assert (= (length coords) dimensions))
      (let* ((base-corner (mapcar #'floor coords))
             ;; Fractional parts of the coordinates, used to interpolate between
             ;; the values at each corner of the cell that this point is in.
             (fs (mapcar (lambda (x)
                           (multiple-value-bind (i f) (floor x)
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
                                     (smoothstep v1 v2 f))
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

(defun generate-gray-code-sequences (n)
  ;; See: https://en.wikipedia.org/wiki/Gray_code#Constructing_an_n-bit_Gray_code
  (labels ((rec (n sequences)
             (if (<= n 0)
                 sequences
                 (rec (1- n)
                      (nconc (mapcar (lambda (sequence) (cons 0 sequence))
                                     sequences)
                             (mapcar (lambda (sequence) (cons 1 sequence))
                                     (reverse sequences)))))))
    ;; Reverse all the sequences at the end because we want them to be
    ;; in the order so that every adjacent values differ in their x coordinate,
    ;; every adjacent pairs differ in their y coordinate, every adjacent quadruples
    ;; differ in their z coordinate, etc.
    (mapcar #'reverse (rec n (list (list))))))
