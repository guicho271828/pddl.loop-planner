#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

#|
  Author: Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner-asd
  (:use :cl :asdf))
(in-package :pddl.loop-planner-asd)

(defsystem pddl.loop-planner
  :version "0.1"
  :author "Masataro Asai"
  :license ""
  :depends-on (:pddl
               :pddl.scheduler
               :pddl.loop-detection
               :guicho-utilities.threading
               :bordeaux-threads
               :lparallel
               :optima
               :iterate
               :alexandria
               :osicat
               :inferior-shell)
  :components ((:file "src/package")
               (:module "src"
                :components
                ((:file "planner")
                 (:file "plan-attributes")
                 (:file "utils")
                 (:file "main"))
                :depends-on ("src/package")))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op pddl.loop-planner-test))))


(defmethod asdf:perform ((op asdf:test-op)
			 (system (eql (asdf:find-system :pddl.loop-planner))))
  (funcall (find-symbol "RUN!" (find-package :fiveam)) :pddl.loop-planner)
  t)