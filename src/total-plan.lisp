(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

;; (((#<PDDL-PLAN {11673AA9}>
;;    #<PDDL-PROBLEM CELL-ASSEMBLY-MODEL2A-1-STEADY-STATE-0> (0)))
;;  105)

@export
(defun build-total-plan (howmany unit-plan base-type loop-plan loop-problem ss)
  "returns plan actions, total-problem, all bases"
  (assert (< (length ss) howmany))
  (let ((*domain* (domain unit-plan))
        (tmpdir (mktemp :total)))
    (multiple-value-bind (*problem* all-bases)
        (build-total-problem howmany base-type unit-plan loop-problem)
      (write-problem *problem* tmpdir)
      (values (build-initial-plan      all-bases base-type 
                                       *problem* loop-problem ss)
              (build-intermediate-plan all-bases base-type
                                       loop-plan loop-problem ss)
              (build-final-plan        all-bases base-type 
                                       *problem* loop-problem ss)
              *problem*
              all-bases))))

@export
(defun gen-base-many (n)
  (let (acc)
    (dotimes (i n acc)
      (push (gen-base (- n i 1)) acc))))


