(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

(defvar *fd-dir* (pathname-as-directory #p"~/repos/downward"))
(defvar *options* "ipc seq-sat-lama-2011")
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
(defun test-problem (problem
		     domain
		     &key
		     (stream *standard-output*)
		     (options *options*)
		     (memory 200000)
		     (time-limit 10))
  (run `(,*test-problem* -m ,memory
                         -t ,time-limit
                         -o ,options
                         ,problem ,domain)
       :show t
       :output stream
       :on-error nil)
  (run `(pipe (find ,(pathname-directory-pathname problem)
                    -maxdepth 1
                    -mindepth 1)
	      (grep (,(pathname-name problem) .plan)))
       :show t
       :output :lines
       :on-error nil))

