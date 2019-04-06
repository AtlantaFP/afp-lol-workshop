(defpackage afp-lol-workshop
  (:use :cl))
(in-package :afp-lol-workshop)

;; blah blah blah.
;; int * allocateAndAssign() {
;;    int * a = malloc(sizeof(int));
;;    *a = 1;
;;   return a;
;; }

(defun environment-with-indefinite-extent (input)
  (cons input nil))

(let ((x 2))
  (print x))

(defun register-allocated-fixnums ()
  "
  Example of looping through fixnums to show how some Lisp compilers optimize
  lexical contexts
  generate efficient assembly. We can actually help
  the compiler by providing extra bits of information like
  "
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (loop :for i fixnum :from 1 :to 100 :do
      (incf (the fixnum acc)
            (the fixnum i)))
    acc))

(defvar *temp-special*)

(setq *temp-special* 1)

(defun print-temp-symbol ()
  "example of a null lexical environment."
  (incf *temp-special*))

;; In the following example we are changing our special variable before
;; invoke our print symbol function. Note when defining a special variable
;; in a let form, any function invoked within that uses the special variable
;; will take the shadowed value provided by the lexical environment in the
;; the let form instead of acutally grabbing the value from the top-level
;;
;; NOTE: special variables can be used to implement storage and variables that
;; are thread-local
(defun example-global-variables ()
  (let ((*temp-special* 2))
    (print-temp-symbol)
    (print *temp-special*))
  *temp-special*)

;; reference Let Over Lambda p.30
(defun compiler-test ()
  "
  Any smart CL compiler will look at the following funcall, recognize it will always
  return 3, and optimize the code to just return 3 instead of making the funcall.
  "
  (funcall
   (lambda (x) (1+ x))
   2))

;; in Common Lisp, a compiled lambda form is a constant form. this means that
;; any references to the compiled lambda are essentially jmp to a particular memory
;; address
(defun lambda-returner ()
  (lambda (x) (* x x)))

(defun make-adder (x)
  (lambda (y) (+ x y)))

;; classic example of a lexical closure (implementing a counter)
(defparameter *increment-counter*
  (let ((counter 0))
   (lambda () (incf counter))))

(let ((direction 'up))
  (defun toggle-direction ()
    (if (eq direction 'up)
        (setf direction 'down)
        (setf direction 'up)))

 (defun make-counter ()
   (let ((counter 0))
     (lambda ()
       (if (eq direction 'up)
           (incf counter)
           (decf counter))))))

