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
  :depends-on (:pddl.loop-planner
               :fiveam)
  :components ((:module "t"
                :components
                ((:file :package)
                 (:file :validate)
                 (:file :test-problem)
                 (:file :total-plan))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.loop-planner)"))
		    (asdf:clear-system c)))
