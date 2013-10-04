(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defvar *base-limit*)

@export
(defun my-handler (c)
  (let ((ss (steady-state c)))
    ;; (break+ ss
    ;;         (length ss)
    ;;         (force *base-limit*)
    ;;         (find-restart 'skip-this))
    (if (< (force *base-limit*) (length ss))
        (invoke-restart (find-restart 'wind-stack))
        (continue))))
