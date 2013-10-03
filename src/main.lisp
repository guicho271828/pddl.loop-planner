(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

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

(defun test-problem-and-get-plan (base-type ppath &key
                                  (memory 200000000)
                                  (time-limit 15))
  (let* ((*problem* (%make-problem ppath))
         (*domain* (domain *problem*))
         (plans
          (mapcar
           #'%make-plan
           (test-problem ppath (path *domain*)
                         :stream nil
                         :memory memory
                         :time-limit time-limit)))
         (analyses (mapcar (rcurry #'analyze-plan base-type) plans)))
    (report-results ppath analyses)
    (values *problem* plans analyses)))

(defun get-plans (base-type problem-pathnames &key
                  (howmany 40)
                  (memory 200000000) ;; 200 MB
                  (time-limit 15)
                  lazy)
  (let ((total 0)
        (all-problem-searched-once-p nil)
        (rb-lock (make-lock "Red Black Tree lock"))
        (rb-queue (leaf))
        (lazy-paths-lock (make-lock "Lazy paths lock"))
        (result-lock (make-lock "Result lock"))
        (loop-plan-results (make-hash-table)))
    (restart-return ((finish (lambda ()
                               (multiple-value-list (rb-minimum rb-queue)))))
      (do-restart ((run-more
                    (lambda (n) (setf howmany n))
                    :interactive-function #'query-integer)
                   (set-search-time
                    (lambda (n)
                      (setf howmany 0)
                      (setf time-limit n))
                    :interactive-function #'query-integer)
                   (set-max-memory
                    (lambda (n)
                      (setf howmany 0)
                      (setf memory n))
                    :interactive-function #'query-integer))
        (pdotimes (i howmany)
          @ignorable i
          (handler-return ((type-error
                            (lambda (c)
                              @ignore c
                              (setf all-problem-searched-once-p t)
                              nil)))
            (multiple-value-bind (*problem* plans analyses)
                (test-problem-and-get-plan
                 base-type
                 (if lazy
                     (with-lock-held (lazy-paths-lock)
                       (fpop problem-pathnames))
                     (elt problem-pathnames (+ total i)))
                 :time-limit time-limit
                 :memory memory)
              (with-lock-held (result-lock)
                (setf (gethash *problem* loop-plan-results) plans))
              (iter (for plan in plans)
                    (for (seq par base-count time-per-base) in analyses)
                    (with-lock-held (rb-lock)
                      (setf rb-queue
                            (rb-insert
                             rb-queue par
                             (cons (list plan *problem*)
                                   (rb-member par rb-queue)))))))))
        (incf total howmany)
        
        (multiple-value-bind (content value)
            (rb-minimum rb-queue)
          (error "What to do next? ~:[~;All problems are already searched at least once.~]
Problems searched ~40t= ~a
Current min. parallelized cost ~40t= ~5,2f
Corresponding plan,problem ~40t=
  ~w
Current time limit ~40t= ~a
Current memory limit ~40t= ~a"
                 all-problem-searched-once-p
                 total value content time-limit memory))))))

@export
(defun exploit-and-solve-loop-problems (unit-plan base-object
                                        &rest rest
                                        &key
                                        (howmany 40)
                                        (memory 200000000)
                                        (time-limit 15)
                                        lazy)
  (declare (ignorable howmany memory time-limit))
  (multiple-value-bind (paths base-type)
      (if lazy
          (exploit-loop-problems-lazy unit-plan base-object)
          (exploit-loop-problems unit-plan base-object))
    (apply #'get-plans base-type paths rest)))


