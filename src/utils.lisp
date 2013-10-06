(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

(defun %make-problem (ppath)
  (let ((*package* (find-package :pddl.instances)))
    (symbol-value
     (handler-bind ((found-in-dictionary
                     #'muffle-warning))
       (parse-file ppath)))))

(defun %make-plan (path)
  (let ((*package* (find-package :pddl.instances)))
    (pddl-plan :path path)))

(defun %shorthand-pathname (path &optional (within 20))
  (let* ((str (concatenate 'string (pathname-name path) "." (pathname-type path)))
         (len (length str))
         (back (- len within))
         (start (if (minusp back) 0 back)))
    (concatenate 'string "..." (subseq str start len))))

(defun query-integer ()
  (loop
     (format *query-io* "~&Input a positive integer. : ")
     (let ((read (read *query-io*)))
       (when (and (integerp read) (plusp read))
         (return (list read))))))

