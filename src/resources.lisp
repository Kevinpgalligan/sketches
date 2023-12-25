(in-package sketches)

(defun load-static-resource (filename)
  "Loads a static resource from src/static/<name-of-sketch-package>/<filename>."
  (let ((base-dir (slot-value (asdf:find-system 'sketches)
                              'asdf/component:absolute-pathname))
        (suffix (sketch-pkg-suffix)))
    (when (not suffix)
      (error "Not in a sketch package!"))
    (load-resource
     (namestring
      (uiop:merge-pathnames*
       (pathname filename)
       (uiop:merge-pathnames*
        (pathname (concatenate 'string (string-downcase suffix) "/"))
        (uiop:merge-pathnames* #p"static/" base-dir)))))))
