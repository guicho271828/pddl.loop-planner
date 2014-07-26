
(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

(test validate
  (let ((*default-pathname-defaults*
         (asdf:system-relative-pathname
          :pddl.loop-planner-test "t/data")))
    (is-true
      (validate-plan "domain.pddl"
                     "problem.pddl"
                     "opt"))
    (is-false
     (validate-plan "domain.pddl"
                    "problem.pddl"
                    "opt.dummy"))))
