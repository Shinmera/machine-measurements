(in-package #:org.shirakumo.machine-state.measurements)

(defstruct (measurement
            (:conc-name NIL)
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL))
  (last-time 0.0d0 :type double-float)
  (last-value 0 :type T))

(defmethod print-object ((measurement measurement) stream)
  (print-unreadable-object (measurement stream :type T :identity T)))

(defgeneric measure (measurement))

(defmethod measure :around ((measurement measurement))
  (let* ((value (call-next-method))
         (time (org.shirakumo.precise-time:get-monotonic-time/double))
         (tdiff (- time (last-time measurement))))
    (setf (last-time measurement) time)
    (values value tdiff measurement)))

(defstruct (diff-measurement
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)
            (:include measurement)))

(defmethod measure :around ((measurement diff-measurement))
  (multiple-value-bind (value tdiff) (call-next-method)
    (let ((vdiff (- value (last-value measurement))))
      (setf (last-value measurement) value)
      (values vdiff tdiff measurement))))

(defstruct (rate-measurement
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)
            (:include diff-measurement)))

(defmethod measure :around ((measurement rate-measurement))
  (multiple-value-bind (vdiff tdiff) (call-next-method)
    (values (/ vdiff tdiff) tdiff measurement)))

(defmacro define-measurement (name slots &body measure)
  (destructuring-bind (name &optional (super 'measurement))
      (if (listp name) name (list name))
    (let ((constructor (intern (format NIL "%~a" name)))
          (initargs (loop for slot in slots
                          unless (consp slot) collect slot)))
      `(progn
         (defstruct (,name
                     (:constructor ,constructor ,initargs)
                     (:copier NIL)
                     (:predicate NIL)
                     (:include ,super))
           ,@slots)

         (defun ,name ,initargs
           (let ((,name (,constructor ,@initargs)))
             (measure ,name)
             ,name))

         (defmethod print-object ((,name ,name) stream)
           (print-unreadable-object (,name stream :type T :identity T)
             (format stream "~@{~a~^ ~}"
                     ,@(loop for slot-name in initargs
                             collect `(,(intern (format NIL "~a-~a" name slot-name)) ,name)))))
         
         (defmethod measure ((,name ,name))
           (symbol-macrolet ,(loop for slot in slots
                                   for slot-name = (if (listp slot) (car slot) slot)
                                   collect `(,slot-name (,(intern (format NIL "~a-~a" name slot-name)) ,name)))
             ,@measure))))))
