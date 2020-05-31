(defpackage :afp-lol-workshop.readtables
  (:use :cl)
  (:export
   #:reader-macros
   ))

(in-package :afp-lol-workshop.readtables)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop :for i :from 1 :to numarg
		    :collect (alexandria:symbolicate 'a (write-to-string i)))
       ,(funcall
	 (get-macro-character #\`) stream nil)))

  (named-readtables:defreadtable :reader-macros
    (:merge :current)
    (:dispatch-macro-char #\# #\` #'|#`-reader|)
    (:case :upcase)))

