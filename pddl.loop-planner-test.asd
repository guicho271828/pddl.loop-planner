#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-test-asd
  (:use :cl :asdf))
(in-package :pddl.loop-planner-test-asd)

(defsystem pddl.loop-planner-test
  :author "Masataro Asai"
  :license ""
  :depends-on (:pddl.instances
               :pddl.instances.cell-assembly-loop
	       :pddl.loop-planner
               :pddl-test
               :repl-utilities
               :fiveam
               :log4cl)
  :components ((:module "t"
                :components
                ((:file :pddl.loop-planner)
                 (:file :validate)
                 (:file :total-plan))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.loop-planner)"))
		    (asdf:clear-system c)))
