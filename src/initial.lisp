(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

(defvar *initial-plan-fd-option*
  "ipc seq-sat-lama-2011")

@export
(defparameter *memory-limit-for-initial/final*
  (parse-integer
   (run/s `(pipe (free)
                 (tr -s " ")
                 (grep "buffers/cache")
                 (cut :delimiter " " :f 3))))
  "A memory limit [kB] for the initial and final search.
This value can be modified with (setf *memory-limit-for-initial/final* newval).")

@export
(defvar *hard-time-limit-for-initial/final*
  1800
  "A hard time limit [sec] for the initial and final search.
This value can be modified with (setf *hard-time-limit-for-initial/final* newval).")

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
          (write-problem initial-problem (mktemp (concatenate-symbols
                                                  :initial (length all-bases))))
          (path *domain*)
          :time-limit 300
          :hard-time-limit *hard-time-limit-for-initial/final*
          :memory *memory-limit-for-initial/final*
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
            (initial-bases-not-to-move (nthcdr (length ss) all-bases))
            (base-type-p (rcurry #'pddl-typep base-type))
            (loop-bases (remove-if-not base-type-p objs))
            (init (categorize 
                   init :key 
                   (lambda (state)
                     (if (some (rcurry #'related-to state) loop-bases) t nil))))
            (init/bases (gethash nil init))
            (init+bases (gethash t   init)))
       
       (pddl-problem :domain *domain*
                     :name (concatenate-symbols loop-name 'initial (length all-bases))
                     :objects (set-difference
                               (objects/const total-problem)
                               initial-bases-not-to-move)
                     :init (remove-if
                            (lambda (state)
                              (some (rcurry #'related-to state)
                                    initial-bases-not-to-move))
                            (init total-problem))
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
