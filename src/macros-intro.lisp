(defpackage afp-lol.macros-intro
  (:use :cl)
  (:nicknames "macros-intro"))
(in-package :afp-lol.macros-intro)
;; macro basics
(defun sleep-minutes (m)
  (sleep (* m 60)))

#|
When first starting with macros it might be useful to start with a function.
In this example, we are trying to create a general sleep function that takes
two parameters:
- amount of time to sleep
- units of measure the value is written in
As you can see in  this example, you will need to quote the units before passing
it into the function as a parameter because CL will otherwise treats the unit as
variable, perform a lookup and either throw an error that it is unbound or worse
pass in the wrong value into the function

USAGE:
> (sleep-units% 5 's) ;; correct usage
> (sleep-units% 5 s) ;; incorrect usage
|#
(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
        ((s) 1)
        ((m) 60)
        ((h) 3600)))))

(defmacro unit-of-time (value unit)
  `(* ,value
       ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600))))

(defmacro sleep-units (value unit)
  `(sleep (unit-of-time ,value ,unit)))

;; reference : LoL p. 45
(defmacro nlet (n letargs &body body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
    (if (zerop n)
        1
        (* n (fact (- n 1))))))

(defun nlet-fact-tr (n)
  (nlet fact ((n n)
              (acc 1))
    (if (zerop n)
        acc
        (fact (- n 1) (* acc n)))))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &body body)
  (let ((syms (remove-duplicates (remove-if-not
                                  #'g!-symbol-p
                                  (alexandria:flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

;; second attempt
(defmacro/g! nif (expr pos zero neg)
    `(let ((,g!result ,expr))
       (cond ((plusp ,g!result) ,pos)
             ((zerop ,g!result) ,zero)
             (t ,neg))))

;; third attempt
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       ,(let ((expr g))
          `(cond ((plusp ,expr) ,pos)
                 ((zerop ,expr) ,zero)
                 (t ,neg))))))

