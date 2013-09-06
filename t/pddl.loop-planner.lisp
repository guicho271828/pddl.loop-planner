#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-test
  (:use :cl
	:alexandria
        :osicat
        :guicho-red-black-tree
	:pddl
        :pddl.loop-planner
	:pddl.scheduler
	:pddl.instances
        :fiveam))
(in-package :pddl.loop-planner-test)

(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(defvar loop-plan-results
  (make-hash-table))

(defvar parallelized-loop-plan-results
  (leaf))

(defvar domain-directory
  (asdf:system-relative-pathname
   :pddl.loop-planner
   #p"CELL-ASSEMBLY/"))

(defvar base-type
  (type (object cell-assembly-model2a-1 'b-0)))

(defvar domain-pathname (merge-pathnames #p"domain.pddl" domain-directory))
(defvar problem-pathnames
  (remove-if (lambda (path)
               (or (equal path domain-pathname)
                   (not (string= "pddl" (pathname-type path)))))
             (list-directory domain-directory)))

(defun parallel-length (tas)
  (timed-state-time (timed-action-end (lastcar tas))))

(test (test-problem-and-get-plans)
  (dotimes (i 8)
    (let* ((ppath (random-elt problem-pathnames))
           (*domain* cell-assembly)
           (*problem*
            (let ((*package* (find-package :pddl.instances)))
              (symbol-value
               (handler-bind ((found-in-dictionary
                               #'muffle-warning))
                 (parse-file ppath)))))
           (plans
            (mapcar
             (lambda (path) (pddl-plan :path path))
             (test-problem ppath domain-pathname :stream nil))))
      
      (setf (gethash *problem* loop-plan-results) plans)
      
      (dolist (plan plans)
        (let* ((seq-length
                (cost (simulate-plan
                       (pddl-environment :plan plan))))
               (tas (reschedule plan :minimum-slack))
               (parallel-length (parallel-length tas))
               (base-count (count-if (rcurry #'pddl-typep base-type)
                                     (objects *problem*)))
               (time-per-base (/ parallel-length base-count)))
          (is (typep tas 'list))
          (is (numberp seq-length))
          (is (numberp parallel-length))
          (is (<= parallel-length seq-length))
          (is (< 1 base-count))
          (format
           t "

Parallel plan: sequencial ~a parallel ~a base ~a time/base ~5,2f

"
           seq-length parallel-length base-count time-per-base)

          (setf parallelized-loop-plan-results
                (rb-insert
                 parallelized-loop-plan-results
                 time-per-base
                 (cons (list plan *problem*)
                       (rb-member
                        time-per-base
                        parallelized-loop-plan-results)))))))))
                   