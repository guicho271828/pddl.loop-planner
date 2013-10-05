(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

(defvar *fd-dir* (pathname-as-directory #p"~/repos/downward"))

@export
(defvar *fd-options* "ipc seq-sat-lama-2011")
(defvar *translate*
  (merge-pathnames "src/translate/translate.py" *fd-dir*))
(defvar *preprocess*
  (merge-pathnames "src/preprocess/preprocess" *fd-dir*))
(defvar *search*
  (merge-pathnames "src/search/downward" *fd-dir*))

(defvar *system*
  (pathname-as-directory 
   (asdf:system-source-directory :pddl.loop-planner)))

(defvar *test-problem*
  (merge-pathnames "test-problem.sh" *system*))

@export
@doc " Runs
   {(asdf:system-source-directory :pddl.loop-planner)}/test-problem.sh
with the following arguments.

  problem, domain : the pathnames of pddl files.
  options : a string which will be the search and heuristic options to downward.
  memory : number[Byte], given to ulimit -m
  time-limit : number[sec.], given to ulimit -t

returns a list of pathnames of plan files.
"
(defun test-problem (problem
                     domain
                     &key
                     (stream *shared-output*)
                     (options *fd-options*)
                     memory
                     time-limit)
  (run `(,*test-problem* ,@(when memory `(-m ,memory))
                         ,@(when time-limit `(-t ,time-limit))
                         ,@(when options `(-o ,options))
                         ,problem ,domain)
       :show t
       :output stream
       :on-error nil)
  (sort (run `(pipe (find ,(pathname-directory-pathname problem)
                          -maxdepth 1
                          -mindepth 1)
                    (grep (,(pathname-name problem) .plan)))
             :show t
             :output :lines
             :on-error nil)
        #'string<))

