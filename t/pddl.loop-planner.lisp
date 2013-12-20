#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-test
  (:use :cl
        :repl-utilities
        :iterate
	:alexandria
        :osicat
        :guicho-utilities
        :guicho-utilities.threading
        :guicho-red-black-tree
        :bordeaux-threads
        :lparallel
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

(package-optimize-setting)
(optimize*)

#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(defun sleep-and-say-hi ()
  (sleep (random 1.5))
  (with-lock-held (*print-lock*)
    (format *shared-output* "~%Hi! I am ~a !" (current-thread)))
  t)

;;;; parallelization test

(test (test-threaded-test)
  (let ((t1 (make-thread #'sleep-and-say-hi))
        (t2 (make-thread #'sleep-and-say-hi)))
    (print t1)
    (print t2)
    (join-thread t1)
    (join-thread t2)
    (pass "~%finished!")))

(test (future-based-threading)
  (let ((futures
         (iter (for i below 10)
               (for f = (future (sleep-and-say-hi)))
               (print f)
               (collect f))))
    (dolist (f futures)
      (lparallel:force f)
      (format t "~%joined!" )
      (pass))))

;;;; integration tests

(test (get-plan-1)
  (finishes
    (handler-bind ((simple-error
                    (lambda (c)
                      (when-let ((re (find-restart 'finish c)))
                        (invoke-restart re)))))
      (exploit-and-solve-loop-problems
       cell-assembly-model2b-1-1 'b-0
       :howmany 1)))
  (format t "~2%
***************************************~3%
  For more parallelized results, run

    (exploit-and-solve-loop-problems
       cell-assembly-model2b-1-6 'b-0
       :howmany 5)   

***************************************~2%"
          ))


(test (get-plan-1-lazy)
  (finishes
    (handler-bind ((simple-error
                    (lambda (c)
                      (when-let ((re (find-restart 'finish c)))
                        (invoke-restart re)))))
      (exploit-and-solve-loop-problems
       cell-assembly-model2a-1-1 'b-0
       :howmany 1 :lazy t))))

