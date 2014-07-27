(in-package :cl-user)
(defpackage pddl.loop-planner
  (:use :cl
        :alexandria
        :bordeaux-threads
        :cl-rlimit
        :cl-syntax
        :eazylazy 
        :guicho-red-black-tree
        :guicho-utilities
        :inferior-shell
        :iterate
        :optima 
        :osicat
        :pddl
        :pddl.loop-detection
        :pddl.plan-optimizer
        :pddl.scheduler
        :ppcre)
  (:shadow :minimize :maximize)
  (:shadowing-import-from :eazylazy :force :delay))
