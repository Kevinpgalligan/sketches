(in-package sketches)

(defparameter *sketch-pkg-prefix* "KG.SKETCH.")

(defun get-sketch-package-name (name)
  (concatenate 'string *sketch-pkg-prefix* (symbol-name name)))

(defmacro def-sketch-package (name) 
  "Creates a package called {*SKETCH-PKG-PREFIX*}.{NAME} to contain code
specific to a particular sketch."
  `(defpackage ,(make-symbol (get-sketch-package-name name))
     (:use :cl :sketch :sketches)))

(defun run-sketch (name &rest rest)
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