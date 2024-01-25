(in-package sketches)

(defstruct lsystem name rules default-axiom)

(defmacro def-lsystem (name rules &optional default-axiom)
  `(setf (getf (symbol-plist ',name) :lsystem)
         (make-lsystem :name ',name
                       :rules ',rules
                       :default-axiom ,default-axiom)))

(defun evaluate-lsystem (name &key axiom (rounds 1))
  (let* ((lsystem (getf (symbol-plist name) :lsystem))
         (current (or axiom
                      (lsystem-default-axiom lsystem)
                      (error "Axiom must be provided for this L-system."))))
    (loop repeat rounds
          while current
          do (let ((new (list)))
               (loop for sym across current
                     do (let ((rule
                                (find-matching-rule sym (lsystem-rules lsystem))))
                          (if (null rule)
                              (push (list sym) new)
                              (push (rule-expansion rule) new))))
               (setf current (apply #'concatenate 'string (nreverse new)))))
    current))

(defun find-matching-rule (sym rules)
  (assoc sym rules))

(defun rule-expansion (rule)
  (second rule))
