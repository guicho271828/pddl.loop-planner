(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defun build-total-problem (howmany base-type unit-plan loop-problem)
  (let ((unit-problem (problem unit-plan))
        (loop-name (name loop-problem)))
    (ematch unit-problem
      ((pddl-problem :domain *domain*
                     :init init
                     :goal goal
                     :metric metric)
       (let* ((base-type-p (rcurry #'pddl-typep base-type))
              (objects (categorize (objects/const unit-problem)
                                   :key base-type-p))
              (objects/bases (gethash nil objects))
              (bases         (gethash t   objects))
              (init (categorize 
                     init :key 
                     (lambda (state)
                       (if (some (rcurry #'related-to state) bases) t nil))))
              (init/bases (gethash nil init))
              (init+bases (gethash t   init))
              (goal (categorize 
                     (positive-predicates goal) :key 
                     (lambda (state)
                       (if (some (rcurry #'related-to state) bases) t nil))))
              (goal/bases (gethash nil goal))
              (goal+bases (gethash t   goal)))
         (assert (= (length bases) 1))
         (let ((*problem*
                (pddl-problem
                 :name (concatenate-symbols loop-name 'total)
                 :domain *domain*
                 :init init/bases
                 :goal (list* 'and goal/bases)
                 :metric metric))
               (all-bases
                (mapcar (curry #'pddl-object :type base-type :name)
                        (gen-base-many howmany))))

           ;; add objects
           (setf (objects *problem*)
                 (append objects/bases all-bases))

           ;; add init description
           (appendf (init *problem*)
                    (mappend (lambda (state)
                               (mapcar (lambda (new)
                                         (substitute-objects state (list new) bases))
                                       all-bases))
                             init+bases))

           ;; add goal description
           (appendf (goal *problem*)
                    (mappend (lambda (state)
                               (mapcar (lambda (new)
                                         (substitute-objects state (list new) bases))
                                       all-bases))
                             goal+bases))
           
           (values *problem* all-bases)))))))
