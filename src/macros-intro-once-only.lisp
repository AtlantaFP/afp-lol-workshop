(defpackage afp-lol.macros-intro-once-only
  (:use :cl)
  (:nicknames "macros-intro-once-only"))
(in-package :afp-lol.macros-intro-once-only)

;
#|
Agenda
- understanding once-only macro
- some final discussions on our macro intro review.
- reader macros

discussion of different splicing operations
- , (unquote)
- ., (unquote into cdr)
- ,@ (splice)
- ,. (destructive splice)

|#

(let ((lst '(b c d)))
  `(a . ,lst))


(defun safer-use-of-bq ()
  (let ((lst '(b c d)))
    (print `(a
       ,.(mapcar #'identity lst)
p       e))
    lst))

#|
Before getting started on understanding once-only we need to define a few terms that
were introduced last time:
    - free variables
    - variable capture
    - unwanted variable capture
|#

;;; Free variables

;; following (when x is not bound) is an example to a free variable
(let ((x 1))
  (print (1+ x)))


#|
Macros leverage two main things
- free variables
- extending lexical context
|#

(defmacro free-variable-injector ()
  'x)

;;; once-only (code sample from Ch.8 of Practical Common Lisp)
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

(defmacro square (x)
  (once-only (x)
    `(* ,x ,x)))

(let ((x 1))
  (print (square (incf x))))

;; example of what NOT to do (side-effect unexpected)
(print (loop :for i :from 1 :to 5 :collect (square i)))
