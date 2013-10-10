
(in-package :pddl.loop-planner-test)

(defun benchmark ()
  (log:config :daily *log-name*)
  (log:info "start planning")
  (log:info "initial/final soft time limit [sec] : ~a " 300)
  (log:info "initial/final hard time limit [sec] : ~a " *hard-time-limit-for-initial/final*)
  (log:info "initial/final memory limit [kB]     : ~a " *memory-limit-for-initial/final*)
  (log:info "loop soft time limit [sec]          : ~a " 5)
  (log:info "loop hard time limit [sec]          : ~a " 120)
  (log:info "loop memory limit [kB]              : ~a " 500000)
  (log:info "bases-per-loop limit                : ~a " 3)
  (log:info "max. steady states per problem      : ~a " 50)
  (iter (for plansym in '(cell-assembly-model2b-1-6
                          cell-assembly-model2a-1-6
                          cell-assembly-model3a-1-5
                          cell-assembly-model3b-1-5
                          cell-assembly-model3c-1-8))
        (for plan = (symbol-value plansym))
        (log:info "planning ~a" plansym)
        (iter (for i in '(4 16 64 256 1024))
              (log:info "planning ~a base plan" i)
              (for result = (multiple-value-list 
                             (solve-many-problems
                              i plan 'b-0
                              :howmany 50
                              :time-limit 120
                              :memory 500000
                              :base-limit 3
                              :interactive nil
                              :directory *log-dir*)))
              (log:info "result:" result)
              (for validation = (apply #'validate-plan result))
              (log:info "Validation result:" validation))))