(in-package :cl-user)
(defpackage pddl.loop-planner
  (:use :cl :pddl :pddl.scheduler :pddl.loop-detection
        :optima :iterate :alexandria :osicat :inferior-shell
        :guicho-utilities.threading
        :lparallel)
  (:shadow :minimize :maximize))