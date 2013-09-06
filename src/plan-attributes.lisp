
(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun sequencial-length (plan)
  @type pddl-plan plan
  (cost (simulate-plan
         (pddl-environment :plan plan))))

@export
(defun parallel-length (plan)
  (timed-state-time
   (timed-action-end
    (lastcar (reschedule plan :minimum-slack)))))

@export
(defun count-objects (problem base-type)
  (count-if (rcurry #'pddl-typep base-type)
            (objects problem)))
