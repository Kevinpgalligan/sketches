(in-package sketches)

(defun load-static-resource (filename)
  "Loads a static resource from src/static/<name-of-sketch-package>/<filename>."
  (load-resource filename :with-sketch-package-suffix t))

(defun get-static-path (filename &key with-sketch-package-suffix)
  (let ((base-dir (slot-value (asdf:find-system 'sketches)
                              'asdf/component:absolute-pathname)))
    (let ((static-dir (uiop:merge-pathnames* #p"static/" base-dir)))
      (when with-sketch-package-suffix
        (let ((suffix (or (sketch-pkg-suffix)
                          (error "Not in a sketch package!"))))
          (setf static-dir
                (uiop:merge-pathnames*
                 (pathname (concatenate 'string (string-downcase suffix) "/"))
                 static-dir))))
      (namestring
       (uiop:merge-pathnames* (pathname filename) static-dir)))))
