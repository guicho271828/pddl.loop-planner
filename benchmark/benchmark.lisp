
;; evaluate the contents of this file with slime
;; in order to actually see the result with eyes

(ql:quickload :pddl.loop-planner.benchmark)
(in-package :pddl.loop-planner.benchmark)

(test-problem (path pddl.instances:cell-assembly-model2a-each-1)
              (path pddl.instances:cell-assembly-eachparts)
              :verbose t)

(test-problem (path pddl.instances:cell-assembly-model2a-each-1)
              (path pddl.instances:cell-assembly-eachparts)
              :options *opt-options*
              :verbose t)

(let ((*domain* pddl.instances:cell-assembly-eachparts)
      (*problem* pddl.instances:cell-assembly-model2a-each-1))
  (time
   (exploit-loop-problems
    (pddl-plan :path "/mnt/video/guicho/repos/lisp/pddl/data/model2a-each/p0001.plan.1")
    (list (object *problem* :b-0)
          (object *problem* :part-a-0)
          (object *problem* :part-b-0)
          (object *problem* :part-c-0))
    #'evaluate-loop-problem
    :verbose nil
    :soft-timeout-per-ss 5
    :hard-timeout-per-ss 20
    :timeout 300)))

(let ((*domain* pddl.instances:cell-assembly-eachparts)
      (*problem* pddl.instances:cell-assembly-model2a-each-1))
  (time
   (let ((*default-special-bindings*
          (cons `(*fd-options* . ,*opt-options*)
                *default-special-bindings*)))
     (exploit-loop-problems
      (pddl-plan :path "/mnt/video/guicho/repos/lisp/pddl/data/model2a-each/p0001.plan.1")
      (list (object *problem* :b-0)
            (object *problem* :part-a-0)
            (object *problem* :part-b-0)
            (object *problem* :part-c-0))
      #'evaluate-loop-problem
      :verbose t
      :timeout 30))))

(let ((*domain* pddl.instances:cell-assembly-eachparts)
      (*problem* pddl.instances:cell-assembly-model2a-each-1))
  (time
   (let ((*default-special-bindings*
          (cons `(*fd-options*
                  . ,(wrap-option "--search lazy_wastar(lmcut(),w=2)"))
                *default-special-bindings*)))
     (exploit-loop-problems
      (pddl-plan :path "/mnt/video/guicho/repos/lisp/pddl/data/model2a-each/p0001.plan.1")
      (list (object *problem* :b-0)
            (object *problem* :part-a-0)
            (object *problem* :part-b-0)
            (object *problem* :part-c-0))
      #'evaluate-loop-problem
      :verbose t
      :timeout 300))))

pddl.instances:cell-assembly-2a2b-mixed-each-1
pddl.instances:cell-assembly-model2a-each-1
pddl.instances:cell-assembly-eachparts
