
(ql:quickload :pddl.loop-planner.benchmark)
(in-package :pddl.loop-planner.benchmark)

(test-problem (path pddl.instances:cell-assembly-model2a-each-1)
              (path pddl.instances:cell-assembly-eachparts)
              :verbose t)

(let ((*domain* pddl.instances:cell-assembly-eachparts)
      (*problem* pddl.instances:cell-assembly-model2a-each-1))
  (format t "~{~&~a~%~}"
          (multiple-value-list
           (exploit-loop-problems
            (pddl-plan :path "/mnt/video/guicho/repos/lisp/pddl/data/model2a-each/p0001.plan.1")
            (list (object *problem* :b-0)
                  (object *problem* :part-a-0)
                  (object *problem* :part-b-0)
                  (object *problem* :part-c-0))
            #'evaluate-loop-problem
            :verbose t
            :timeout 30))))

pddl.instances:cell-assembly-2a2b-mixed-each-1
pddl.instances:cell-assembly-model2a-each-1
pddl.instances:cell-assembly-eachparts
