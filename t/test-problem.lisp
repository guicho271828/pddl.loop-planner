
(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

;; tests with-timeout and run-program

(test test-problem
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


(test (exploit-loop-problems-with-evaluation
       :depends-on test-problem)
  (time (let ((*domain* pddl.instances:cell-assembly-eachparts)
              (*problem* pddl.instances:cell-assembly-model2a-each-1))
          (exploit-loop-problems
           (pddl-plan :path "/mnt/video/guicho/repos/lisp/pddl/data/model2a-each/p0001.plan.1")
           (list (object *problem* :b-0)
                 (object *problem* :part-a-0)
                 (object *problem* :part-b-0)
                 (object *problem* :part-c-0))
           #'evaluate-loop-problem
           :verbose t
           :timeout 30))))
