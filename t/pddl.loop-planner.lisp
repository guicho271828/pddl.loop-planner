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
        :bordeaux-threads
	:pddl
        :pddl.loop-planner
	:pddl.scheduler
	:pddl.instances
        :fiveam)
  (:shadow :maximize :minimize))
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

(defun %make-problem (ppath)
  (let ((*package* (find-package :pddl.instances)))
    (symbol-value
     (handler-bind ((found-in-dictionary
                     #'muffle-warning))
       (parse-file ppath)))))

(defun %make-plan (domain problem path)
  (let ((*package* (find-package :pddl.instances)))
    (pddl-plan :domain domain
               :problem problem
               :path path)))

(defvar *rb-lock* (make-lock "Red Black Tree lock"))
(defun %rb-queue-safe (x xc)
  (with-lock-held (*rb-lock*)
    (setf parallelized-loop-plan-results
          (rb-insert
           parallelized-loop-plan-results
           x 
           (cons xc
                 (rb-member
                  x
                  parallelized-loop-plan-results))))))

(defvar *print-lock* (make-lock "IO Stream lock"))
(defvar *shared-output* *standard-output*)
(defun sleep-and-say-hi ()
  (sleep (random 3))
  (with-lock-held (*print-lock*)
    (format *shared-output* "~%Hi! I am ~a !" (current-thread)))
  t)

(test (test-threaded-test)
  (let ((t1 (make-thread #'sleep-and-say-hi))
        (t2 (make-thread #'sleep-and-say-hi)))
    (print t1)
    (print t2)
    (join-thread t1)
    (join-thread t2)
    (pass "~%finished!")))


(defun sleep-and-say-hi-limited ()
  (with-limited-cores ()
    (sleep (random 3))
    (with-lock-held (*print-lock*)
      (format *shared-output* "~%Hi! I am ~a !" (current-thread)))
    t))

(test (limited-core)
  (let ((threads
         (iter (for i below 10)
               (for th = (make-thread #'sleep-and-say-hi-limited
                                      :name (format nil "Test limited-core thread ~a" i)))
               (print th)
               (collect th))))
    (dolist (th threads)
      (join-thread th)
      (format t "~%thread ~a joined!" (thread-name th))
      (pass))))

(defvar *result-lock* (make-lock "Result lock"))
(defun test-problem-and-get-plan ()
  (let ((ppath (random-elt problem-pathnames)))
    (with-limited-cores (300)
      (with-lock-held (*print-lock*)
        (format *shared-output*
                "~%Thread ~a Solving ~a ..."
                (thread-name (current-thread)) (pathname-name ppath)))
      (let* ((domain cell-assembly)
             (problem (%make-problem ppath))
             (plans
              (mapcar
               (curry #'%make-plan domain problem)
               (test-problem ppath domain-pathname :stream nil))))
        
        (with-lock-held (*result-lock*)
          (setf (gethash problem loop-plan-results) plans))
        
        (dolist (plan plans)
          (let* ((seq (sequencial-length plan problem domain))
                 (par (parallel-length plan))
                 (base-count (count-objects problem base-type))
                 (time-per-base (/ par base-count)))
            
            ;; (with-lock-held (*test-lock*)
            ;;   (is (<= par seq))
            ;;   (is (< 1 base-count)))
            (with-lock-held (*print-lock*)
              (format
               *shared-output* "~%~{~10<~a~>~}~%~{~10<~5,2f~>~}"
               '(seq. par. base time/base seq./par.)
               (list seq par base-count time-per-base (/ seq par))))
            (%rb-queue-safe time-per-base (list plan problem))))))))

(test (get-plan-1)
  (finishes
    (test-problem-and-get-plan)))

(defun get-plans ()
  (let (threads (low 0) (high 40))
    (restart-return ((destroy-all-threads
                      (lambda ()
                        (mapcar #'destroy-thread threads)
                        nil)))
      (do-restart ((run-20-times-more
                    (lambda ()
                      (incf low 20)
                      (incf high 20))))
        (iter (for i from low below high)
              (push (make-thread
                     #'test-problem-and-get-plan
                     :name (format nil "Test ~4:<#~a~>" i))
                    threads))
        (mapcar #'join-thread threads)))))

