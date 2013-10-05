(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)


(defvar *validate* (merge-pathnames #p"src/validate" *fd-dir*))

@export
(defun validate-plan (domain problem plan &key
                      verbose
                      (stream *standard-output*))
  (handler-return ((error (constantly nil)))
    (run (if verbose
             `(,*validate* -v ,domain ,problem ,plan)
             `(,*validate* ,domain ,problem ,plan))
         :show t
         :output stream)
    t))

