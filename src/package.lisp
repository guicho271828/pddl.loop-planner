(in-package :cl-user)
(defpackage pddl.loop-planner
  (:use :cl :pddl :pddl.scheduler :pddl.loop-detection
        :optima :iterate :alexandria :osicat :inferior-shell
        :guicho-utilities.threading
        :guicho-utilities
        :guicho-red-black-tree
        :bordeaux-threads
        :annot.doc
        :lparallel)
  (:shadow :minimize :maximize)
  (:shadowing-import-from :pddl.loop-detection :force))