(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)


@export
(defun build-intermediate-plan (all-bases
                                base-type
                                loop-plan loop-problem ss)
  (let ((len (length ss))
        (bases (remove-if-not (rcurry #'pddl-typep base-type) (objects loop-problem)))
        (actions (actions loop-plan)))
    (iter total
          (for rest-bases on all-bases)
          (for current-loop-bases = (subseq rest-bases 0 len))
          (iter (for a in-vector actions from 1 below (1- (length actions)))
                (in total
                    (collecting (substitute-objects a current-loop-bases bases)))))))

(defun substitute-objects (a new-bases old-bases)
  (assert (= (length new-bases) (length old-bases)))
  (ematch a
    ((and (pddl-parametrized-object :parameters params)
          (namable :name name))
     (make-instance
      (class-of a)
      :name name :parameters
      (let ((params (copy-list params)))
        (iter (for b1 in old-bases)
              (for b2 in new-bases)
              (setf params (nsubstitute b2 b1 params)))
        params)))))
