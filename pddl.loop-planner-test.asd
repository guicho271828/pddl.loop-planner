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
	       :pddl.loop-planner
               :pddl-test
               :repl-utilities
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "pddl.loop-planner"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
