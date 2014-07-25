
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)

@export
(defun evaluate-loop-problem (problem ss schedule movements component)
  (let ((tmpdir (mktemp :pddl)))
    (write-problem
     (apply #'loop-problem problem ss schedule movements component)
     tmpdir)
    


(test exploit-loop-problems-with-evaluation
  (finishes
    (let ((*domain* make) (*problem* makep))
      (print
       (multiple-value-list
        (exploit-loop-problems
         (pddl-plan :actions (parse-plan +makeplan+))
         (object *problem* :p1)
         #'evaluate-loop-problem
         :verbose t))))))

