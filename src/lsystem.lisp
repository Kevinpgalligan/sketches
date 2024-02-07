(in-package sketches)

(defstruct lsystem name rules default-axiom)

(defmacro def-lsystem (name rules &optional default-axiom)
  "Define an L-system! NAME is a symbol denoting the name, RULES is a list
of lists, where the first element of each list is a character (symbol/terminal
/ whatever), and the second element is a string, representing the expansion of
that character. DEFAULT-AXIOM should be a string."
  `(setf (getf (symbol-plist ',name) :lsystem)
         (make-lsystem :name ',name
                       :rules ',rules
                       :default-axiom ,default-axiom)))

(defun evaluate-lsystem (name eval-fn &key axiom (depth 1))
  "NAME is a symbol, the name of the L-system. EVAL-FN takes one argument, a
character, and does something. DEPTH determines at what depth of the expansion
to apply the eval function. And AXIOM is the starting string -- if not provided,
the default axiom of the L-system is used, if available."
  (let* ((lsystem (getf (symbol-plist name) :lsystem))
         (rules (lsystem-rules lsystem))
         (current (or axiom
                      (lsystem-default-axiom lsystem)
                      (error "Axiom must be provided for this L-system."))))
    (labels ((rec (c current-depth)
               (let ((rule (find-matching-rule c rules)))
                 (if (or (null rule)
                         (>= current-depth depth))
                     (funcall eval-fn c)
                     (loop for c-next across (rule-expansion rule)
                           do (rec c-next (1+ current-depth)))))))
      (loop for c across axiom
            do (rec c 0)))))

(defun find-matching-rule (c rules)
  (assoc c rules))

(defun rule-expansion (rule)
  (second rule))
