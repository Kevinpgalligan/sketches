(in-package sketches)

;; TODO handle cleanup when sketch is closed.
(defclass widgets ()
  ((active-p :initform t :accessor active-p)))

(defmethod initialize-instance :before ((instance widgets) &key &allow-other-keys)
  (when (not (ltk:wish-stream ltk:*wish*))
    (ltk:start-wish)
    ;; This should make sure we don't keep trying to process events
    ;; after LTK window has closed.
    (ltk:on-close nil ;; TODO figure out what to put here
                  (lambda () (setf (active-p instance) nil)))))

(defmethod draw :after ((instance widgets) &key &allow-other-keys)
  ;; TODO make sure wish is still alive
  (when (active-p instance)
    (ltk:process-events)))

(defclass widget ()
  ((value :initarg :value :accessor value)))

(defun slider (&key label (min 1) (max 5)
                 (initial-value (floor (- max min) 2))
                 (resolution 2) action)
  (let* ((f (make-instance 'ltk:frame))
         (widget (make-instance 'widget :value initial-value))
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
                                         (read-from-string v-rounded-as-str nil nil))))))))
    ;; TODO fix this not being displayed correctly from the start.
    (setf (ltk:value scale) initial-value)
    (setf (ltk:text val-label) initial-value)
    (ltk:pack f)
    (when label
      (ltk:pack (make-instance 'ltk:label
                               :master f
                               :text label)
                :side :left))
    (ltk:pack scale :side :left)
    (ltk:pack val-label :side :left)
    widget))
