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

#|
We can certainly do better. By simply moving the sleep-units into a macro,
we can get rid of this need to quote the unit of measure and just simply execute
the generated sleep call with the computed value as a constant
|#
(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
       ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)))))

;; macroexpand the following example
(sleep-units 5 m)

;; even look at the disassembly
;; on SBCL all constants are left-shifted by a bit (looks like they are multiplied by 2)
(disassemble (lambda () (sleep-units 5 m)))

#|
Cost of above abstraction: units of measure no longer can be runtime dispatched.
By making the simple change and generate the unit of time before generating the code
to sleep we can preserve runtime dispatching when we need it without loss of efficiency.
|#
(defmacro unit-of-time (value unit)
  `(* ,value
       ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600))))

(defmacro sleep-units (value unit)
  `(sleep (unit-of-time ,value ,unit)))

#|
Code Reference: Let Over Lambda p. 45

Creation of a new control structure nlet to create named lets like those in Scheme.

Control Structure (p. 47) : A construct that doesn't follow the behavior of a function.
it can change the control flow of a program when executed (in this case once expanded).
|#

(defmacro nlet (n letargs &body body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

;; example of macro usage
(defun nlet-fact (n)
  (nlet fact ((n n))
    (if (zerop n)
        1
        (* n (fact (- n 1))))))

;; tail recursive version of the above code
(defun nlet-fact-tr (n)
  (nlet fact ((n n)
              (acc 1))
    (if (zerop n)
        acc
        (fact (- n 1) (* acc n)))))

#|
"only use macros when functions won't do" (p. 47)
Author points out that using macros make sense when:
- you don't want to evaluate certain arguments
- evaluate argumets out of order
- evaluate more than once 
|#
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

#|
Macros to uniquely generate
|#
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

;;(defmacro defmacro/g! (name args &body body)
;;  (let ((syms (remove-duplicates (remove-if-not
;;                                  #'g!-symbol-p
;;                                  (alexandria:flatten body)))))
;;    `(defmacro ,name ,args
;;       (let ,(mapcar
;;              (lambda (s)
;;                `(,s (gensym ,(subseq
;;                               (symbol-name s)
;;                               2))))
;;              syms)
;;         ,@body))))

;; second attempt
;;(defmacro/g! nif (expr pos zero neg)
;;    `(let ((,g!result ,expr))
;;       (cond ((plusp ,g!result) ,pos)
;;             ((zerop ,g!result) ,zero)
;;             (t ,neg))))

;; third attempt
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       ,(let ((expr g))
          `(cond ((plusp ,expr) ,pos)
                 ((zerop ,expr) ,zero)
                 (t ,neg))))))

