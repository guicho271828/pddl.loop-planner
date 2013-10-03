(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defvar *base-limit* MOST-POSITIVE-FIXNUM)

@export
(defun my-handler (ss)
  (when (< *base-limit* (length ss))
    (break+ ss *base-limit*)
    (invoke-restart (find-restart 'skip-this))))