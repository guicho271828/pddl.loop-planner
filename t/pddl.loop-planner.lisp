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
	:pddl
        :pddl.loop-detection
        :pddl.loop-planner
	:pddl.scheduler
	:pddl.instances
        :fiveam)
  (:shadow :maximize :minimize))
(in-package :pddl.loop-planner-test)

(package-optimize-setting)
(optimize*)

#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
#+sbcl
(declaim (sb-ext:muffle-conditions warning))

(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(defvar domain-directory
  (asdf:system-relative-pathname
   :pddl.loop-planner
   #p"CELL-ASSEMBLY/"))

(defvar base-type
  (type (object cell-assembly-model2a-1 'b-0)))

(defvar domain-pathname (merge-pathnames #p"domain.pddl" domain-directory))

(defparameter tmpdir
  (merge-pathnames (string-downcase (gensym "cell-assebly")) #p"/tmp/"))

(let ((unit-problem cell-assembly-model2a-1)
      (unit-plan cell-assembly-model2a-1-6))
  (let ((schedule (reschedule unit-plan
                              :minimum-slack
                              :verbose t)))
    (multiple-value-bind (movements movements-indices)
        (extract-movements 'b-0 schedule cell-assembly)
      (mapcar (lambda (loop-plan)
                (write-problem
                 (build-steady-state-problem unit-problem
                                             loop-plan
                                             schedule
                                             movements
                                             movements-indices
                                             base-type)
                 tmpdir))
              (progn
                (format t "~3%Exploiting loopable steady-states from the movements.
It takes a long time (> around 4 min), please wait...")
                (sleep 2)
                (time (exploit-loopable-steady-states
                       movements
                       (exploit-steady-states movements)
                       :verbose :modest)))))))

(defparameter problem-pathnames
  (remove-if-not (lambda (path)
                   (string= "pddl" (pathname-type path)))
                 (list-directory
                  (merge-pathnames "CELL-ASSEMBLY"
                                   (pathname-as-directory tmpdir)))))

(defun %make-problem (ppath)
  (let ((*package* (find-package :pddl.instances)))
    (symbol-value
     (handler-bind ((found-in-dictionary
                     #'muffle-warning))
       (parse-file ppath)))))

(defun %make-plan (path)
  (let ((*package* (find-package :pddl.instances)))
    (pddl-plan :path path)))

(defvar *rb-lock* (make-lock "Red Black Tree lock"))
(defvar parallelized-loop-plan-results (leaf))
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

(defun sleep-and-say-hi ()
  (sleep (random 1.5))
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
    (sleep (random 1.5))
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
(defvar loop-plan-results
  (make-hash-table))
(defun test-problem-and-get-plan ()
  (let ((ppath (random-elt problem-pathnames)))
    (with-limited-cores (300)
      (with-lock-held (*print-lock*)
        (format *shared-output*
                "~%Thread ~a Solving ~a ..."
                (thread-name (current-thread)) (pathname-name ppath)))
      (let* ((*domain* cell-assembly)
             (*problem* (%make-problem ppath))
             (plans
              (mapcar
               #'%make-plan
               (test-problem ppath domain-pathname :stream nil))))
        
        (with-lock-held (*result-lock*)
          (setf (gethash *problem* loop-plan-results) plans))
        
        (dolist (plan plans)
          (let* ((seq (sequencial-length plan))
                 (par (parallel-length plan))
                 (base-count (count-objects *problem* base-type))
                 (time-per-base (/ par base-count)))
            
            ;; (with-lock-held (*test-lock*)
            ;;   (is (<= par seq))
            ;;   (is (< 1 base-count)))
            (with-lock-held (*print-lock*)
              (terpri *shared-output*)
              (pprint-logical-block (*shared-output*
                                     nil
                                     :per-line-prefix
                                     (thread-name (current-thread)))
                (format
                 *shared-output* "~%~{~10<~a~>~}~%~{~10<~5,2f~>~}"
                 '(seq. par. base time/base seq./par.)
                 (list seq par base-count time-per-base (/ seq par)))))
            (%rb-queue-safe time-per-base (list plan *problem*))))))))

(test (get-plan-1)
  (finishes
    (test-problem-and-get-plan))
  (format t "~2%
***************************************~3%
  For more parallelized results, run ~w .~3%
***************************************~2%"
          '(progn
            (in-package :pddl.loop-planner-test)
            (get-plans))))

(defun get-plans (&optional (howmany 40))
  (let (threads (low 0) (high howmany))
    (restart-return ((destroy-all-threads
                      (lambda ()
                        (dolist (th threads)
                          (when (thread-alive-p th)
                            (destroy-thread th)))
                        nil)))
      (do-restart ((run-20-times-more
                    (lambda ()
                      (incf low 20)
                      (incf high 20))))
        (iter (for i from low below high)
              (push (make-thread
                     #'test-problem-and-get-plan
                     :name (format nil "~4@<#~a~>" i))
                    threads))
        (mapc #'join-thread threads)
        (multiple-value-bind (content value)
            (rb-minimum parallelized-loop-plan-results)
          (error "what to do next?
problems searched          = ~a
current minumum time/base  = ~5,2f
corresponding plan,problem =
  ~w"
                 high value content))))))

