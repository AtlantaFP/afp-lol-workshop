(defpackage :afp-lol-workshop.type-system-intro
  (:use :cl))

(in-package :afp-lol-workshop.type-system-intro)

(locally
  (declare (optimize (safety 0)))
  (declaim (ftype (function (fixnum) fixnum) add-one))
  (defun add-one (x)
    (declare (fixnum x))
    (1+ x)))

(deftype my-int (a) 'integer)

(deftype hand-counting-integer () '(integer 0 10))

(defun sum-values-in-array (arr)
  (declare (type (simple-array single-float (3)) arr)
           (optimize (speed 3) (safety 0)))
  (loop :for i :across arr :sum i :into res single-float
        :finally (return res)))

(sb-ext:truly-the fixnum "hello world")

(defun test-truly-the (a)
  (declare (optimize (safety 3)))
  (1+ (sb-ext:truly-the fixnum a)))

;;
;; PSA: please don't (declaim (optimize ...)) EVER!!!
;;

