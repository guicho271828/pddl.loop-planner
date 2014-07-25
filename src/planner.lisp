(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

;;;; parameters

(defparameter *fd-dir* (pathname-as-directory #p"~/repos/downward"))
(defvar *fd-options* "ipc seq-sat-lama-2011")
(defvar *translate* (merge-pathnames "src/translate/translate.py" *fd-dir*))
(defvar *preprocess* (merge-pathnames "src/preprocess/preprocess" *fd-dir*))
(defvar *search* (merge-pathnames "src/search/downward" *fd-dir*))
(defvar *system*
  (pathname-as-directory 
   (asdf:system-source-directory :pddl.loop-planner)))
(defparameter *test-problem*
  (merge-pathnames "planner-scripts/test-problem.sh" *system*))
;;;; helpers

@export
(define-condition plan-not-found (warning)
  ((problem-path :initarg :problem-path)
   (domain-path :initarg :domain-path))
  (:report (lambda (c s)
             (with-slots (problem-path domain-path) c
                (format s "Failed to find a plan!~%Domain:~a~%Problem:~a"
                        problem-path domain-path)))))

;; http://www.ymeme.com/slurping-a-file-common-lisp-83.html
(defun slurp (stream)
  (let ((seq (make-array (file-length stream)
                         :element-type 'character
                         :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun read-file (path)
  (with-input-from-file (s path)
    (slurp s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pathname-p (path)
    "defined for optima matcher"
    (pathnamep path)))

(defun elapsed-time (problem kind)
  "Parse the log file and extract the elapsed time in seconds"
  (ematch (pathname problem)
    ((pathname- name directory)
     (register-groups-bind (user-in-seconds)
         ("user ([.0-9]*)"
          (read-file
           (make-pathname
            :type "log"
            :directory directory
            :name (concatenate 'string name "." kind))))
       (read-from-string user-in-seconds)))))

(defun max-memory (problem kind)
  "Parse the log file and extract the elapsed time in kB"
  (ematch (pathname problem)
    ((pathname- name directory)
     (register-groups-bind (user-in-seconds)
         ("maxmem ([.0-9]*)"
          (read-file
           (make-pathname
            :type "log"
            :directory directory
            :name (concatenate 'string name "." kind))))
       (read-from-string user-in-seconds)))))

(defun ulimit (rlimit)
  (case rlimit
    ((:infinity) "unlimited")
    (t rlimit)))

;;;; main function

(declaim (ftype (function (pathname
                           pathname
                           &key
                           (:stream stream)
                           (:options string)
                           (:verbose boolean)
                           (:memory (or integer keyword))
                           (:time-limit (or integer keyword))
                           (:hard-time-limit (or integer keyword)))
                          (values list
                                  real real real
                                  real real real))
                test-problem))

@export
(defun test-problem (problem
                     domain
                     &key
                       (stream *standard-output*)
                       (options *fd-options*)
                       verbose
                       (memory (rlimit +rlimit-address-space+))
                       (time-limit (rlimit +rlimit-cpu-time+))
                       (hard-time-limit (rlimit +rlimit-cpu-time+)))
  " Runs
   {(asdf:system-source-directory :pddl.loop-planner)}/test-problem.sh
with the following arguments.

  problem, domain : the pathnames of pddl files.
  options : a string which will be the search and heuristic options to downward.
  memory : number[kByte], given to ulimit -m
  time-limit : number[sec.], given to ulimit -t

returns:
  a list of pathnames of plan files
  elapsed-times of translate, preprocess, search
  max-memory of translate, preprocess, search"
  (ignore-errors
    (run `(,*test-problem*
           ,@(when verbose `(-v))
           ,@(when memory `(-m ,(ulimit memory)))
           ,@(when time-limit `(-t ,(ulimit time-limit)))
           ,@(when hard-time-limit `("-T" ,(ulimit hard-time-limit)))
           ,@(when options `(-o ,options))
           ,problem ,domain)
         :show t
         :output stream))
  (values
   (sort (block nil
           (run `(pipe (find ,(pathname-directory-pathname problem)
                             -maxdepth 1
                             -mindepth 1)
                       (grep (,(pathname-name problem) .plan)))
                :show t
                :output :lines
                :on-error (lambda (c)
                            (declare (ignore c))
                            (warn 'plan-not-found
                                  :problem-path problem
                                  :domain-path domain)
                            (return nil))))
         #'string<)
   (elapsed-time problem "translate")
   (elapsed-time problem "preprocess")
   (elapsed-time problem "search")
   (max-memory problem "translate")
   (max-memory problem "preprocess")
   (max-memory problem "search")))


