
(in-package :pddl.loop-planner-test)
(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(test total-plan
  (finishes
    (solve-many-problems
     2
     cell-assembly-model2a-1-6
     'b-0
     :howmany 3 :time-limit 20 :base-limit 5)))



