(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

(test elp-with-e-with-component
  (finishes
    (let ((*domain* assemble) (*problem* assemblep))
      (describe
       (exploit-loop-problems
        (pddl-plan :actions (parse-plan +assembleplan+))
        (list (object *problem* :p1)
              (object *problem* :pa11)
              (object *problem* :pa12))
        #'evaluate-loop-problem)))))
