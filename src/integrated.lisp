(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun solve-many-problems (basenum unit-plan unit-object
                            &rest rest
                            &key
                            (howmany 40)
                            (memory 200000000)
                            (time-limit 15)
                            (base-limit MOST-POSITIVE-FIXNUM)
                            (handler #'my-handler))
  (declare (ignorable howmany memory time-limit base-limit handler))
  (multiple-value-bind (result base-type)
      (apply #'exploit-and-solve-loop-problems
             unit-plan unit-object :lazy t rest)
    (apply #'build-total-plan
           basenum
           unit-plan
           base-type
           (first result))
    ;; (multiple-value-bind (initial-actions actions total-problem all-bases)
    
    ;;   )
    ))