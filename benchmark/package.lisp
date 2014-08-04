
#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner.benchmark
  (:use :cl
        :cl-rlimit
        :iterate
	:alexandria
        :bordeaux-threads
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
(in-package :pddl.loop-planner.benchmark)

(defparameter *log-dir*
  (merge-pathnames
   #p"Dropbox/loop-planner/"
   (user-homedir-pathname)))
(ensure-directories-exist *log-dir*)

(defparameter *log-name*
  (merge-pathnames #p"logfile" *log-dir*))
