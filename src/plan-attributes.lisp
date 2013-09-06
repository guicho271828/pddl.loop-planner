
(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun sequencial-length (plan)
  @type pddl-plan plan
  (cost (simulate-plan
         (pddl-environment :plan plan))))

@export
(defun parallel-length (plan)
  (reduce #'max
          (reschedule plan :minimum-slack)
          :key (lambda (ta)
                 (timed-state-time
                  (timed-action-end ta)))
          :initial-value 0))

@export
(defun count-objects (problem base-type)
  (count-if (rcurry #'pddl-typep base-type)
            (objects problem)))
