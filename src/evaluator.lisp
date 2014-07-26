
(in-package :pddl.loop-planner)
(use-syntax :annot)

@export
(defun evaluate-loop-problem (problem ss schedule movements component
                              &key verbose)
  (let* ((*domain* (domain problem))
         (*problem*
          (apply #'loop-problem problem ss schedule movements component))
         (tmpdir
          (mktemp (name *problem*)))
         (domain-path
          (write-pddl *domain* "domain.pddl" tmpdir verbose))
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
                  (reschedule (pddl-plan :path path) :minimum-slack))))))
            :initial-value MOST-POSITIVE-FIXNUM)))

