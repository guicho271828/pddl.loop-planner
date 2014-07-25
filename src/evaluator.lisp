
(in-package :pddl.loop-planner)
(use-syntax :annot)

@export
(defun evaluate-loop-problem (problem ss schedule movements component
                              &key verbose)
  (let* ((*problem*
          (apply #'loop-problem problem ss schedule movements component))
         (tmpdir
          (mktemp (name *problem*)))
         (domain-path
          (write-pddl (domain *problem*) "domain.pddl" tmpdir verbose))
         (problem-path
          (write-pddl *problem* "problem.pddl" tmpdir verbose))
         (plan-path-list
          (test-problem problem-path domain-path :verbose verbose)))
    (reduce #'min plan-path-list :key
            (lambda (path)
              (timed-state-time 
               (timed-action-end
                (lastcar
                 (sort-schedule
                  (reschedule (pddl-plan :actions (parse-plan path))
                              :minimum-slack)))))))))

