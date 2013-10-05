(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

(defvar *final-plan-fd-option*
  "ipc seq-sat-lama-2011")

@export
(defun build-final-plan (all-bases
                         base-type
                         total-problem
                         loop-problem ss)
  
  (let ((final-problem (build-final-problem
                        total-problem
                        loop-problem
                        all-bases base-type ss)))
    (values
     (butlast
      (cdr
       (parse-plan
        (lastcar
         (test-problem
          (write-problem final-problem (mktemp :final))
          (path *domain*)
          :time-limit 300
          :options *final-plan-fd-option*))
        *domain*
        final-problem)))
     final-problem)))

@export
(defun build-final-problem (total-problem loop-problem
                            all-bases base-type ss)
  (ematch loop-problem
    ((pddl-problem :domain *domain*
                   :name loop-name
                   :objects objs
                   :init init
                   :goal goal
                   :metric metric)
     
     (let* ((final-bases-to-move (last all-bases (length ss)))
            (base-type-p (rcurry #'pddl-typep base-type))
            (loop-bases (remove-if-not base-type-p objs))
            (loop-goal+bases (remove-if-not 
                              (lambda (state)
                                (some (rcurry #'related-to state) loop-bases))
                              (positive-predicates goal)))
            
            (loop-init/bases (remove-if
                              (lambda (state)
                                (some (rcurry #'related-to state) loop-bases))
                              init)))
       
       (pddl-problem :domain *domain*
                     :name (concatenate-symbols loop-name 'final)
                     :objects (objects/const total-problem)
                     :init (append loop-init/bases ; ベース以外の条件
                                   (mapcar (lambda (state) ; 最終ループ後に移動するベースら
                                             (substitute-objects
                                              state 
                                              final-bases-to-move
                                              loop-bases))
                                           loop-goal+bases)
                                   (remove-if ; 既に移動後のベースら
                                    (lambda (state)
                                      (some (rcurry #'related-to state)
                                            final-bases-to-move))
                                    (positive-predicates (goal total-problem))))
                     :goal (goal total-problem)
                     :metric metric)))))