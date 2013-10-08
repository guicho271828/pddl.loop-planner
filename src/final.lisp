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
          (write-problem final-problem (mktemp (concatenate-symbols
                                                :final (length all-bases))))
          (path *domain*)
          :time-limit 300
          :memory *memory-limit-for-initial/final*
          :hard-time-limit *hard-time-limit-for-initial/final*
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
            (final-bases-not-to-move (butlast all-bases (length ss)))
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
                     :name (concatenate-symbols loop-name 'final (length all-bases))
                     :objects (set-difference
                               (objects/const total-problem)
                               final-bases-not-to-move)
                     :init (append loop-init/bases ; ベース以外の条件
                                   (remove-if-not ; すべてのベースに関して、function のみ
                                    (lambda (state)
                                      (and (some (rcurry #'related-to state) all-bases)
                                           (typep state 'pddl-function-state)))
                                    (init total-problem))
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
                     :goal (list* 'and
                                  (remove-if
                                   (lambda (state)
                                     (some (rcurry #'related-to state)
                                           final-bases-not-to-move))
                                   (positive-predicates
                                    (goal total-problem))))
                     :metric metric)))))