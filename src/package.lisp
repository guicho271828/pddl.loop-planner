(in-package :cl-user)
(defpackage pddl.loop-planner
  (:use :cl :pddl :pddl.scheduler :pddl.loop-detection
	:optima :iterate :alexandria :osicat :inferior-shell
        :bordeaux-threads
        :bt-semaphore)
  (:shadow :minimize :maximize))