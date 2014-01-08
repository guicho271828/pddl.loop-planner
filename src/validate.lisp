(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)


(defvar *validate* (merge-pathnames #p"src/validate" *fd-dir*))

@export
(defvar *validator-verbosity* nil)

@export
(defun validate-plan (domain-pathname
                      problem-pathname
                      plan-pathname
                      &key
                      (verbose *validator-verbosity*)
                      (stream *standard-output*))
  (handler-return ((error (constantly nil)))
    (run `(,*validate* ,@(when verbose '(-v))
                       ,domain-pathname
                       ,problem-pathname
                       ,plan-pathname)
         :show t
         :output stream)
    t))

