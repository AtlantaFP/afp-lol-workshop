(defpackage :afp.lol.workshop.efficiency-topics
  (:use :cl :sb-alien)
  (:nicknames :efficiency))

(in-package :afp.lol.workshop.efficiency-topics)

;; Some of the following code examples are from Let Over Lambda Ch. 7

;; one might be tempted to do this
(defmacro go-fast ()
  `(declare (optimize (speed 3) (safety 0))))

;; read-macro that implements the same thing above
(set-dispatch-macro-character #\# #\f
                              (lambda (stream sub-char numarg)
                                (declare (ignore stream sub-char))
                                (setf numarg (or numarg 3))
                                (unless (<= numarg 3)
                                  (error "Bad value for #f: ~a" numarg))
                                `(declare (optimize (speed ,numarg)
                                                    (safety ,(- 3 numarg))))))

;; examples of using read macro
'#f
'#0f
'#2f

(defun dum-function ()
  (declare (optimize (speed 3))))

;; regular macros for generating fast and safe areas for expression compilation
(defmacro fast-progn (&body body)
  `(locally #f ,@body))

;; safe version
(defmacro safe-progn (&body body)
  `(locally #0f ,@body))


(defun fast-keywords-strip (args)
  (if args
      (cond
        ((eq (car args) '&key)
         (fast-keywords-strip (cdr args)))
        ((consp (car args))
         (cons (caar args)
                #1=(fast-keywords-strip (cdr args))))
        (t
         (cons (car args) #1#)))))

;;
(defmacro defun-with-fast-keywords (name args &body body)
  (a:with-gensyms (fast-fun rest)
    `(progn
       (defun ,name ,args ,@body)
       (defun ,fast-fun
           ,(fast-keywords-strip args)
         ,@body)
       (compile ',fast-fun)
       (define-compiler-macro ,name (&body ,rest)
         (destructuring-bind ,args ,rest
           (list ',fast-fun ,@(fast-keywords-strip args)))))))

(defun slow-keywords-test (a b &key (c 0) (d 0))
  (+ a b c d))

(defun-with-fast-keywords
    fast-keywords-test (a b &key (c 0) (d 0))
  (+ a b c d))

(defun keywords-benchmark (n)
  (format t "Slow keys: ~%")
  (sb-ext:gc :full t)
  (time
   (loop :repeat n :do
     (slow-keywords-test 1 2 :d 3 :c n)))
  (format t "Fast keys: ~%")
  (sb-ext:gc :full t)
  (time
   (loop :repeat n :do
         (fast-keywords-test 1 2 :d 3 :c n))))

(defun fformat (&rest all)
  (apply #'format all))

(compile 'fformat)

