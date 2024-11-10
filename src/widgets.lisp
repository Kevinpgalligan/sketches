(in-package sketches)

(defclass widgets ()
  ((active-p :initform t :accessor active-p)))

(defun ltk-cleanup (widgets)
  ;; Make sure we don't keep trying to process events
  ;; after the LTK window has closed.
  (when (active-p widgets)
    (setf (active-p widgets) nil)
    (ltk:destroy ltk:*tk*)
    (ltk:exit-wish)))

(defmethod initialize-instance :before ((instance widgets) &key &allow-other-keys)
  (when (not (ltk:wish-stream ltk:*wish*))
    (ltk:start-wish)
    (ltk:on-close ltk:*tk* (lambda () (ltk-cleanup instance)))))

(defmethod kit.sdl2:close-window :before ((instance widgets))
  (ltk-cleanup instance))

(defmethod draw :after ((instance widgets) &key &allow-other-keys)
  (when (active-p instance)
    (ltk:process-events)))

(defclass widget ()
  ((value :initarg :value :accessor value)))

(defun make-widget (v)
  (make-instance 'widget :value v))

(defun slider (&key label (min 1) (max 5)
                 (initial-value (+ min (floor (- max min) 2)))
                 (resolution 2) action)
  (let* ((f (make-instance 'ltk:frame))
         (widget (make-widget initial-value))
         (val-label (make-instance 'ltk:label :master f :text "0"))
         (scale (make-instance
                 'ltk:scale
                 :master f
                 :from min
                 :to max
                 :command (lambda (v)
                            (let ((v-rounded-as-str
                                    (format nil "~v$" resolution v)))
                              (setf (value widget) v
                                    (ltk:text val-label) v-rounded-as-str)
                              (when action
                                (funcall action
                                         ;; Ugly way of doing rounding.
                                         (read-from-string
                                          v-rounded-as-str nil nil))))))))
    (ltk:pack f)
    (when label
      (ltk:pack (make-instance 'ltk:label
                               :master f
                               :text label)
                :side :left))
    (ltk:pack scale :side :left)
    (ltk:pack val-label :side :left)
    (setf (ltk:value scale) initial-value)
    (setf (ltk:text val-label) initial-value)
    widget))

(defun button (&key (label "Button") action)
  (let ((widget (make-widget nil)))
    (ltk:pack
     (make-instance 'ltk:button :text label
                                :command (lambda ()
                                           (setf (value widget) (not (value widget)))
                                           (when action
                                             (funcall action)))))
    widget))

(defclass dropdown-widget ()
  ((ltk-widget :initarg :ltk-widget)
   (options :initarg :options)
   (option-values :initarg :option-values :initform nil)))

(defmethod value ((instance dropdown-widget))
  (with-slots (ltk-widget options option-values) instance
    (let ((option (ltk:text ltk-widget)))
      (if option-values
          (elt option-values (get-option-value option options option-values))
          option))))

(defun get-option-value (option options option-values)
  (loop for o in options
        for v in option-values
        when (string= option o)
          return v))

(defun dropdown (options &key option-values)
  (let ((ltk-widget (make-instance 'ltk:combobox
                                   :values options
                                   :text (car options)
                                   :state "readonly")))
    (ltk:pack ltk-widget)
    (make-instance 'dropdown-widget
                   :ltk-widget ltk-widget
                   :options options
                   :option-values option-values)))

;; TODO
;;   - scales w/ integer values
;;   - number inputs (textbox)
;;   - bigger window by default...
;;     or at least, pack with enough padding.
;;   - more resilient when shutting down / starting up
;;     (it's erroring out every time)
