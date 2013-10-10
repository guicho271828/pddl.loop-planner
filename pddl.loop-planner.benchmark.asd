#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(defsystem pddl.loop-planner.benchmark
  :author "Masataro Asai"
  :license ""
  :depends-on (:pddl.instances
               :pddl.instances.cell-assembly-loop
	       :pddl.loop-planner
               :pddl.loop-planner-test
               :pddl-test
               :repl-utilities
               :fiveam
               :log4cl)
  :components ((:module "t"
                :components
                ((:file :benchmark))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
