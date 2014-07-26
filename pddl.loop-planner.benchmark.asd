#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(defsystem pddl.loop-planner.benchmark
  :author "Masataro Asai"
  :license ""
  :depends-on (:pddl.loop-planner
               :pddl.instances
               :pddl.instances.cell-assembly-loop
               :pddl.instances.cell-assembly-eachparts
               :fiveam
               :log4cl)
  :components ((:module :benchmark
                :components
                ((:file :package)
                 (:file :benchmark))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
