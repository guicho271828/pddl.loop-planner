(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

(defvar *initial-plan-fd-option*
  "ipc seq-sat-lama-2011")


@export
(defun build-initial-plan (all-bases
                           base-type
                           total-problem
                           loop-problem ss)
  
  (let ((initial-problem (build-initial-problem
                          total-problem
                          loop-problem
                          all-bases base-type ss)))
    (values
     (butlast
      (cdr
       (parse-plan
        (lastcar
         (test-problem
          (write-problem initial-problem (mktemp :initial))
          (path *domain*)
          :time-limit 300
          :memory 500000
          :options *initial-plan-fd-option*))
        *domain*
        initial-problem)))
     initial-problem)))

@export
(defun build-initial-problem (total-problem loop-problem
                              all-bases base-type ss)
  (ematch loop-problem
    ((pddl-problem :domain *domain*
                   :name loop-name
                   :objects objs
                   :init init
                   :metric metric)
     
     (let* ((initial-bases-to-move (subseq all-bases 0 (length ss)))
            (base-type-p (rcurry #'pddl-typep base-type))
            (loop-bases (remove-if-not base-type-p objs))
            (init (categorize 
                   init :key 
                   (lambda (state)
                     (if (some (rcurry #'related-to state) loop-bases) t nil))))
            (init/bases (gethash nil init))
            (init+bases (gethash t   init)))
       
       (pddl-problem :domain *domain*
                     :name (concatenate-symbols loop-name 'initial)
                     :objects (objects/const total-problem)
                     :init (init total-problem)
                     :goal (list* 'and
                                  (remove-if
                                   (rcurry #'typep 'pddl-function-state)
                                   (append init/bases
                                          (mapcar (lambda (state)
                                                    (substitute-objects
                                                     state 
                                                     initial-bases-to-move
                                                     loop-bases))
                                                  init+bases))))
                     :metric metric)))))
