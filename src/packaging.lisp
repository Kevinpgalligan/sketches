;;;; Namespacing for all my sketches.

(in-package sketches)

(defparameter *sketch-pkg-prefix* "KG.SKETCH.")
(defparameter *all-sketches* (list))

(defun get-sketch-package-name (name)
  (concatenate 'string *sketch-pkg-prefix* (symbol-name name)))

(defmacro def-sketch-package (name) 
  "Creates a package called {*SKETCH-PKG-PREFIX*}.{NAME} to contain code
specific to a particular sketch."
  `(progn
     (when (not (member ',name *all-sketches*))
       (push ',name *all-sketches*))
     (defpackage ,(make-symbol (get-sketch-package-name name))
       (:use :cl :sketch :sketches))))

(defun print-all-sketches ()
  (loop for sk in *all-sketches*
        do (format t "~a " (string-downcase (symbol-name sk))))
  (terpri))

(defun load-sketch (name &rest rest)
  "Runs the sketch associated with the package called NAME.
Extra arguments to the MAKE-INSTANCE call may also be passed.
Currently, there is no support for defining multiple sketches per package."
  (let* ((pkg (find-package (get-sketch-package-name name)))
         (sketch-class
           (loop for sc in (closer-mop:class-direct-subclasses (find-class 'sketch))
                 when (equalp pkg (symbol-package (class-name sc)))
                   return sc)))
    (if sketch-class
        (apply #'make-instance sketch-class rest)
        (error "No sketch exists within the named package."))))

(defun sketch-pkg-suffix ()
  "Returns suffix of the package of the currently-running sketch, or NIL
if it's not in a sub-package. For example, for the package 'kg.sketches.trees.oo'
it will return 'trees'. Shouldn't call it the 'suffix', in that case, but oh well."
  (let ((name (package-name
               (symbol-package
                (class-name
                 (class-of (get-last-sketch))))))
        (prefix-len (length *sketch-pkg-prefix*)))
    (if (string= *sketch-pkg-prefix*
                 (subseq name 0 prefix-len))
        (let ((full-suffix (subseq name prefix-len)))
          ;; If suffix is 'trees.oo', then we want to return
          ;; just 'trees'.
          (first (str:split "." full-suffix)))
        nil)))

(defun get-last-sketch ()
  ;; Cheating by using the internal symbol *SKETCH*.
  sketch::*sketch*)
