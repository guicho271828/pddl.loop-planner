(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
@doc "Returns domain-path,problem-path,plan-path"
(defun solve-many-problems (basenum unit-plan unit-object
                            &rest rest
                            &key
                            (howmany 40)
                            (memory 200000000)
                            (time-limit 15)
                            (base-limit MOST-POSITIVE-FIXNUM)
                            (handler #'my-handler)
                            (interactive t)
                            (directory (user-homedir-pathname)))
  (declare (ignorable howmany memory time-limit base-limit handler))

  (if interactive
      (multiple-value-bind (result base-type)
          (apply #'exploit-and-solve-loop-problems
                 unit-plan unit-object :lazy t rest)
        (multiple-value-call #'output-entire-results
          (apply #'build-total-plan
                 basenum
                 unit-plan
                 base-type
                 (first result))
          directory))
      (handler-bind ((query-finish
                      (lambda (c)
                        @ignore c
                        (invoke-restart (find-restart 'finish)))))
        (setf (getf rest :interactive) t)
        (apply #'solve-many-problems basenum unit-plan unit-object rest))))




@export
(defun output-entire-results (initial-actions
                              intermediate-actions
                              final-actions
                              total-problem
                              all-bases
                              tmpdir)
  "returns plan-path,problem-path,validation-result. "
  @ignorable all-bases
  (write-problem total-problem tmpdir)
  (let ((plan-path (write-actions-as-plan
                    (append initial-actions
                            intermediate-actions
                            final-actions)
                    total-problem
                    tmpdir))
        (problem-path (write-problem total-problem tmpdir)))
    (values (path (domain total-problem))
            problem-path
            plan-path)))
