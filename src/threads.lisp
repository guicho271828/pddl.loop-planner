
(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)


@export
(defun get-core-num ()
  (length
   (run `(pipe (cat /proc/cpuinfo)
               (grep processor))
        :output :lines)))

@export
(defvar *cores* (make-semaphore :name "Free CPU cores" :count (get-core-num)))

@export
(defun call-with-limited-cores (timeout fn)
  (when (wait-on-semaphore *cores* :timeout timeout)
    (unwind-protect
         (funcall fn)
      (signal-semaphore *cores* 1))))

@export
(defmacro with-limited-cores ((&optional timeout) &body body)
  `(call-with-limited-cores
    ,timeout
    (lambda ()
      ,@body)))

