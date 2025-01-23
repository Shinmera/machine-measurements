(in-package #:org.shirakumo.machine-state.measurements)

(defstruct (measurement
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL))
  (last-time 0.0d0 :type double-float))

(defmethod print-object ((measurement measurement) stream)
  (print-unreadable-object (measurement stream :type T :identity T)))

(defgeneric measure (measurement))

(defmethod measure :around ((measurement measurement))
  (let* ((value (call-next-method))
         (time (org.shirakumo.precise-time:get-monotonic-time/double))
         (tdiff (- time (measurement-last-time measurement))))
    (setf (measurement-last-time measurement) time)
    (values value tdiff measurement)))

(defstruct (diff-measurement
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)
            (:include measurement))
  (last-measurement 0 :type (or double-float (unsigned-byte 64))))

(defmethod measure :around ((measurement diff-measurement))
  (multiple-value-bind (value tdiff) (call-next-method)
    (let ((vdiff (- value (diff-measurement-last-measurement measurement))))
      (setf (diff-measurement-last-measurement measurement) value)
      (values vdiff tdiff measurement))))

(defstruct (rate-measurement
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)
            (:include measurement))
  (last-measurement 0 :type (or double-float (unsigned-byte 64))))

(defmethod measure :around ((measurement rate-measurement))
  (multiple-value-bind (value tdiff) (call-next-method)
    (let ((vdiff (- value (rate-measurement-last-measurement measurement))))
      (setf (rate-measurement-last-measurement measurement) value)
      (values (/ vdiff tdiff) tdiff measurement))))

(defmacro define-measurement (name slots &body measure)
  (destructuring-bind (name &optional (super 'measurement))
      (if (listp name) name (list name))
    (let ((constructor (intern (format NIL "%~a" name))))
      `(progn
         (defstruct (,name
                     (:constructor ,constructor ,(mapcar #'car slots))
                     (:copier NIL)
                     (:predicate NIL)
                     (:include ,super))
           ,@slots)

         (defun ,name ,(mapcar #'car slots)
           (let ((,name (,constructor ,@(mapcar #'car slots))))
             (measure ,name)
             ,name))

         (defmethod print-object ((,name ,name) stream)
           (write (list ',name
                        ,@(loop for (slot-name) in slots
                                collect `(,(intern (format NIL "~a-~a" name slot-name)) ,name)))
                  :stream stream))
         
         (defmethod measure ((,name ,name))
           (let ,(loop for (slot-name) in slots
                       collect `(,slot-name (,(intern (format NIL "~a-~a" name slot-name)) ,name)))
             ,@measure))))))
