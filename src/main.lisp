(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun exploit-and-solve-loop-problems (unit-plan base-object
                                        &rest rest
                                        &key
                                        (howmany 40)
                                        (memory 200000000)
                                        (time-limit 15)
                                        (base-limit MOST-POSITIVE-FIXNUM)
                                        lazy
                                        (handler #'my-handler)
                                        &allow-other-keys)
  (declare (ignorable howmany memory base-limit time-limit lazy handler))
  (multiple-value-bind (paths base-type)
      (if lazy
          (exploit-loop-problems-lazy unit-plan base-object)
          (exploit-loop-problems unit-plan base-object))
    (values (apply #'get-plans base-type paths rest)
            base-type)))

@export
(defvar *total*)
@export
(defvar *howmany*)
@export
(defvar *memory*)
@export
(defvar *time-limit*)

;;;; shell function that serves interactive feature
(defun get-plans (base-type problem-pathnames &key
                  (howmany 40)
                  (memory 200000000)
                  (time-limit 15)
                  (base-limit MOST-POSITIVE-FIXNUM)
                  lazy
                  (handler #'my-handler)
                  &allow-other-keys)
  (let ((total 0))
    (restart-bind ((run-more
                    (lambda (n) (setf howmany n) (continue))
                     :interactive-function #'query-integer)
                   (set-search-time
                    (lambda (n)
                      (setf time-limit n)
                      (continue))
                     :interactive-function #'query-integer)
                   (set-max-memory
                    (lambda (n)
                      (setf memory n) (continue))
                     :interactive-function #'query-integer)
                   (set-base-limit
                    (lambda (n)
                      (setf base-limit n) (continue))
                     :interactive-function #'query-integer))
      (handler-bind ((error (lambda (c) (declare (ignore c)) (setf howmany 0))))
        (incf total howmany)
        (let* ((set-special (lambda (worker-loop)
                              (llet ((*total* total)
                                     (*howmany* howmany)
                                     (*memory* memory)
                                     (*time-limit* time-limit)
                                     (*base-limit* base-limit))
                                (funcall worker-loop))))
               (*kernel* (make-kernel
                          (kernel-worker-count)
                          :context set-special)))
          (funcall set-special
                   (lambda ()
                     (get-plans-inner base-type handler
                                      lazy problem-pathnames))))))))


(defun build-search-option (seq-minimum-per-unit ss)
  (if seq-minimum-per-unit
      (lama-2011-bound
       (* seq-minimum-per-unit
          (length ss)))
      *fd-options*))

(defun get-plans-inner (base-type handler lazy problem-pathnames)
  (handler-bind ((steady-state-condition handler))
    (forcef problem-pathnames))
  (let ((all-problem-searched-once-p nil)
        (not-searched-actually 0)
        (rb-lock (make-lock "red black tree lock"))
        (rb-queue (leaf))
        (lazy-paths-lock (make-lock "Lazy paths lock"))
        (result-lock (make-lock "Result lock"))
        (loop-plan-results (make-hash-table))
        (seq-minimum-per-unit MOST-POSITIVE-FIXNUM)) ; the minimum cost of sequencial plans
    (restart-return ((finish (lambda () (rb-minimum rb-queue))))
      (do-restart ((continue (constantly nil)))
        (pdotimes (i (force *howmany*))
          @ignorable i
          (ematch (if lazy
                      (with-lock-held (lazy-paths-lock)
                        (handler-bind ((steady-state-condition handler))
                          (fpop problem-pathnames)))
                      (handler-return ((type-error (constantly nil)))
                        (elt problem-pathnames (+ (force *total*) i))))
            ((list path ss)
             (multiple-value-bind (*problem* plans analyses)
                 (test-problem-and-get-plan
                  base-type path
                  :time-limit (force *time-limit*)
                  :memory (force *memory*))
               (with-lock-held (result-lock)
                 (setf (gethash *problem* loop-plan-results) plans))
               (iter (for plan in plans)
                     (for (seq par base-count time-per-base) in analyses)
                     (with-lock-held (rb-lock)
                       (setf seq-minimum-per-unit 
                             (min seq seq-minimum-per-unit))
                       (setf rb-queue
                             (rb-insert
                              rb-queue par
                              (cons (list plan *problem* ss)
                                    (rb-member par rb-queue))))))))
            (nil 
             (setf all-problem-searched-once-p t)
             (incf not-searched-actually))))
        (pause-and-report 
         rb-queue all-problem-searched-once-p
         (- (force *total*) not-searched-actually)
         (force *base-limit*) (force *time-limit*) (force *memory*))))))

(defun pause-and-report (rb-queue 
                         all-problem-searched-once-p
                         total base-limit time-limit memory)
  (multiple-value-bind (content value)
      (rb-minimum rb-queue)
    (do-restart ((report-best
                  (lambda (n)
                    (print-timed-action-graphically
                     (reschedule (first (nth (1- n) content)) :minimum-slack)
                     *shared-output*))
                  :interactive-function
                  (lambda ()
                    (format *query-io* "Enter the number of which plan to visualize.")
                    (funcall #'query-integer))))
        (error "What to do next? ~:[~;All problems are already searched at least once.~]
Problems searched ~40t= ~a
Current min. parallelized cost ~40t= ~5,2f
~:{
  Corresponding plan~40t= ~w
  Corresponding problem~40t= ~w
  Corresponding ss~40t= ~w
~^or~}
Current base limit ~40t= ~a
Current time limit ~40t= ~a
Current memory limit ~40t= ~a"
               all-problem-searched-once-p
               total value
               content base-limit time-limit memory))))

(defun test-problem-and-get-plan (base-type ppath &key
                                  (memory 200000000)
                                  (time-limit 15)
                                  (options *fd-options*))
  (let* ((*problem* (%make-problem ppath))
         (*domain* (domain *problem*))
         (plans
          (mapcar
           #'%make-plan
           (test-problem ppath (path *domain*)
                         :stream nil
                         :memory memory
                         :time-limit time-limit
                         :options options)))
         (analyses (mapcar (rcurry #'analyze-plan base-type) plans)))
    (report-results ppath analyses)
    (values *problem* plans analyses)))


(defun report-results (ppath analyses)
  (with-lock-held (*print-lock*)
    (terpri *shared-output*)
    (pprint-logical-block (*shared-output*
                           nil
                           :per-line-prefix
                           (format nil "~a : "
                                   (%shorthand-pathname ppath 13)))
      (iter (initially
             (format *shared-output*
                     "~%Solved ~a~%"
                     (%shorthand-pathname ppath 75))
             (format *shared-output*
                     "~%~{~10@<~a~>~^ | ~}"
                     '(sequencial parallel base time/base seq./par.))
             (format *shared-output*
                     "~%~{~10,,,'-@<~a~>~^-+-~}"
                     '(- - - - -)))
            (for (seq par base-count time-per-base) in analyses)
            (format
             *shared-output* "~%~{~10<~5,2f~>~^ | ~}"
             (list seq par base-count time-per-base (/ seq par)))
            (finally
             (format *shared-output*
                     "~%~{~10,,,'-@<~a~>~^-+-~}"
                     '(- - - - -)))))))

(defun analyze-plan (plan base-type)
  (let* ((seq (sequencial-length plan))
         (par (parallel-length plan))
         (base-count (count-objects *problem* base-type))
         (time-per-base (/ par base-count)))
    (list seq par base-count time-per-base)))
