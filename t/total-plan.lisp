
(in-package :pddl.loop-planner-test)
(def-suite :pddl.loop-planner :in :pddl)
(in-suite :pddl.loop-planner)

(test total-plan
  (finishes
    (let ((tmpdir (mktemp :total)))
      (solve-many-problems
       10
       cell-assembly-model3a-1-5
       'b-0 :howmany 2 :time-limit 15 :base-limit 3 :interactive nil
       :directory tmpdir)
      (run `(rm -r ,tmpdir)))))

(defparameter *log-dir*
  (merge-pathnames
   #p"Dropbox/loop-planner/"
   (user-homedir-pathname)))
(ensure-directories-exist *log-dir*)

(defparameter *log-name*
  (merge-pathnames #p"logfile" *log-dir*))

(log:config :daily *log-name*)
  
(defun benchmark ()
  (iter (for plan in (list cell-assembly-model2b-1-6
                           cell-assembly-model2a-1-6
                           cell-assembly-model3a-1-5
                           cell-assembly-model3b-1-5
                           cell-assembly-model3c-1-8))

        (log:info "start planning ~a" (name plan))
        (iter (for i in '(4 16 64 256 1024))
              (log:info "start planning ~a base plan" i)
              (log:info "result:"
                        (multiple-value-list 
                         (solve-many-problems
                          i plan 'b-0
                          :howmany 40 :time-limit 120
                          :base-limit 3 :interactive nil
                          :directory *log-dir*))))))

