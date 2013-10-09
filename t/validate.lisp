
(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

(test validate
  (finishes
    (validate-plan (asdf:system-relative-pathname
                    :pddl.loop-planner-test
                    "t/domain.pddl")
                   (asdf:system-relative-pathname
                    :pddl.loop-planner-test
                    "t/pfile1")
                   (asdf:system-relative-pathname
                    :pddl.loop-planner-test
                    "t/pfile1.plan.1")))
  (signals error
    (validate-plan (asdf:system-relative-pathname
                    :pddl.loop-planner-test
                    "t/domain.pddl")
                   (asdf:system-relative-pathname
                    :pddl.loop-planner-test
                    "t/pfile1")
                   (asdf:system-relative-pathname
                    :pddl.loop-planner-test
                    "t/pfile1.plan.1.dummy"))))
