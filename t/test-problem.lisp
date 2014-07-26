(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

(test :test-problem
  (let ((*default-pathname-defaults*
         (asdf:system-source-directory :pddl.loop-planner-test)))
    (multiple-value-bind (plan-path-list
                          translate-time
                          preprocess-time
                          search-time)
        (test-problem
         (merge-pathnames "t/data/problem.pddl")
         (merge-pathnames "t/data/domain.pddl"))
      (is (not (null plan-path-list)))
      (is (numberp (print translate-time)))
      (is (numberp (print preprocess-time)))
      (is (numberp (print search-time))))))


(test exploit-loop-problems-with-evaluation
  (finishes
    (let ((*domain* make) (*problem* makep))
      (format t "~{~&~a~%~}"
       (multiple-value-list
        (exploit-loop-problems
         (pddl-plan :actions (parse-plan +makeplan+))
         (object *problem* :p1)
         #'evaluate-loop-problem))))))
