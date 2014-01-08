(in-package :cl-user)
(defpackage pddl.loop-planner
  (:use :cl :pddl :pddl.scheduler :pddl.loop-detection
        :pddl.plan-optimizer
        :optima :iterate :alexandria :osicat :inferior-shell
        :guicho-utilities.threading
        :guicho-utilities
        :guicho-red-black-tree
        :bordeaux-threads
        :annot.doc
        :eazylazy
        :lparallel)
  (:shadow :minimize :maximize)
  (:shadowing-import-from :eazylazy :force :delay))