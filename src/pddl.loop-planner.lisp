#|
  This file is a part of pddl.loop-planner project.
  Copyright (c) 2013 Masataro Asai
|#

(in-package :cl-user)
(defpackage pddl.loop-planner
  (:use :cl :pddl :pddl.scheduler :pddl.loop-detection :pddl.instances
  :optima :iterate :alexandria :osicat :inferior-shell))
(in-package :pddl.loop-planner)

(cl-syntax:use-syntax :annot)
;; blah blah blah.


@export
(defun write-problem (problem &optional (basedir (asdf:system-source-directory
						  :pddl.loop-planner)))
  (let ((path 
	 (merge-pathnames
	  (format nil "~a/~a" (name (domain problem)) (name problem))
	  basedir)))
    (ensure-directories-exist path :verbose t)
    (print path)
    (with-open-file (s path
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (print-pddl-object problem s))))


(defvar *fd-dir* #p"~/repos/downward")
(defvar *options* "ipc seq-sat-lama-2011")
(defvar *translate* "$FD_DIR/src/translate/translate.py")
(defvar *preprocess* "$FD_DIR/src/preprocess/preprocess")
(defvar *search* "DIR=$FD_DIR/src/search")
(defvar *search* "$SEARCH_DIR/downward $OPTIONS")
