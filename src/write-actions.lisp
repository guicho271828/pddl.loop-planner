(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun write-actions-as-plan (actions problem 
                              &key
                                (basedir (user-homedir-pathname))
                                verbose)
  (let ((path 
         (merge-pathnames
          (let ((*print-escape* nil))
            (format nil "~a/~a.plan.total"
                    (name (domain problem))
                    (name problem)))
          (pathname-as-directory basedir))))
    (ensure-directories-exist path :verbose verbose)
    (when verbose
      (format t "~&Writing ~a~&" path))
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (iter (for a in actions)
            (write `(,(name a) ,@(mapcar #'name (parameters a)))
                   :stream s
                   :escape nil)
            (terpri s)))
    path))
