
(in-package :pddl.loop-planner-test)
(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(test total-plan
  (finishes
    (multiple-value-bind (result base-type)
        (exploit-and-solve-loop-problems
         cell-assembly-model2a-1-6 'b-0
         :howmany 1 :lazy t :base-limit 3)
      (multiple-value-bind (actions total-problem all-bases)
          (apply #'build-total-plan
                 2
                 cell-assembly-model2a-1-6
                 base-type
                 (first result))
        ))))

