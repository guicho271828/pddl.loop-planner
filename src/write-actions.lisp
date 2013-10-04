(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun write-actions-as-plan (actions problem 
                              &optional (basedir (user-homedir-pathname)))
  (let ((path 
         (merge-pathnames
          (let ((*print-escape* nil))
            (format nil "~a/~a.plan.total"
                    (name (domain problem))
                    (name problem)))
          (pathname-as-directory basedir))))
    (ensure-directories-exist path :verbose t)
    (format t "~&Writing ~a~&" path)
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (iter (for a in actions)
            (print `(,(name a) ,@(mapcar #'name (parameters a))) s)))
    path))