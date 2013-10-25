(in-package :pddl.loop-planner-test)

(defun benchmark2 ()
  (log:config :daily *log-name*)
  (log:info "start planning woodworking-loop and barman")
  (log:info "initial/final soft time limit [sec] : ~a " 300)
  (log:info "initial/final hard time limit [sec] : ~a " *hard-time-limit-for-initial/final*)
  (log:info "initial/final memory limit [kB]     : ~a " *memory-limit-for-initial/final*)
  (log:info "loop soft time limit [sec]          : ~a " 5)
  (log:info "loop hard time limit [sec]          : ~a " 120)
  (log:info "loop memory limit [kB]              : ~a " 500000)
  (log:info "bases-per-loop limit                : ~a " 3)
  (log:info "max. steady states per problem      : ~a " 50)
  (iter (for plansym in '(wood-loop-tempo-converted-1-1
                          wood-loop2-tempo-converted-1-1
                          barman-loop-base-2))
        (for plan = (symbol-value plansym))
        (for basesym in '(part-0 part-0 shot1))
        (log:info "planning ~a" plansym)
        (iter (for i in '(4 16 64 256 1024))
              (log:info "planning ~a base plan" i)
              (for result = (multiple-value-list 
                             (solve-many-problems
                              i plan basesym
                              :howmany 50
                              :time-limit 120
                              :memory 500000
                              :base-limit 3
                              :interactive nil
                              :directory *log-dir*)))
              (log:info "result:" result)
              (for validation = (apply #'validate-plan result))
              (log:info "Validation result:" validation))))