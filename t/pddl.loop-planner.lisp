#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-test
  (:use :cl
        :pddl.loop-planner
        :fiveam))
(in-package :pddl.loop-planner-test)

(def-suite :pddl.loop-planner)
(in-suite :pddl.loop-planner)

