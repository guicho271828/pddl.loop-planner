
(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

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

;; (test total-plan
;;   (finishes
;;     (let ((tmpdir (mktemp :total)))
;;       (solve-many-problems
;;        10
;;        cell-assembly-model3a-1-5
;;        'b-0 :howmany 2 :time-limit 15 :base-limit 3 :interactive nil
;;        :directory tmpdir)
;;       (inferior-shell:run `(rm -r ,tmpdir)))))


