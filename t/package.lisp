#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-test
  (:use :cl
        :iterate
	:alexandria
        :osicat
        :guicho-utilities
        :guicho-red-black-tree
        :inferior-shell
        :optima
	:pddl
        :pddl.loop-detection
        :pddl.loop-planner
	:pddl.scheduler
	:pddl.instances
        :fiveam)
  (:shadow :fail :place :maximize :minimize :force :run :!))
(in-package :pddl.loop-planner-test)
(def-suite :pddl.loop-planner)
