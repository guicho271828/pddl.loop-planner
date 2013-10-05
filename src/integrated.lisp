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
    (multiple-value-call #'output-entire-results
      (apply #'build-total-plan
             basenum
             unit-plan
             base-type
             (first result)))))


@export
(defun output-entire-results (initial-actions
                              intermediate-actions
                              final-actions
                              total-problem
                              all-bases)
  "returns plan-path,problem-path,validation-result. "
  @ignorable all-bases
  (let ((tmpdir (mktemp :total)))
    (let ((plan-path (write-actions-as-plan
                      (append initial-actions
                              intermediate-actions
                              final-actions)
                      total-problem
                      tmpdir))
          (problem-path (write-problem total-problem tmpdir)))
      (values plan-path
              problem-path
              (validate-plan (path (domain total-problem))
                             problem-path
                             plan-path :verbose t)))))
