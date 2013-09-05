#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-test
  (:use :cl
	:alexandria
	:pddl
        :pddl.loop-planner
	:pddl.scheduler
	:pddl.instances
        :fiveam))
(in-package :pddl.loop-planner-test)

(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(defvar loop-plan-results)

(let ((*package* (find-package :pddl.instances)))
    (let ((name (parse-file #p"/home/guicho/repos/CELL-ASSEMBLY/CELL-ASSEMBLY-MODEL2A-1-STEADY-STATE-0-1.pddl")))
      (export name)))

(test test-problem-and-get-plans
  (setf loop-plan-results
	(mapcar
	 (lambda (path)
	    (pddl-plan :domain cell-assembly
		       :problem (symbol-value
				 (find-symbol "CELL-ASSEMBLY-MODEL2A-1-STEADY-STATE-0-1"))
		       :path path))
	 (test-problem #p"/home/guicho/repos/CELL-ASSEMBLY/CELL-ASSEMBLY-MODEL2A-1-STEADY-STATE-0-1.pddl"
		       #p"/home/guicho/repos/CELL-ASSEMBLY/domain.pddl"))))