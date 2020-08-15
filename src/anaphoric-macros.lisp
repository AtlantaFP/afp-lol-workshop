(defpackage :afp-lol-workshop.anaphoric-macros
  (:use :cl)
  (:nicknames :anaphoric-macros)
  (:export
   #:|#`-reader|
   #:self
   #:alet
   #:alet%
   #:alambda))

(in-package :afp-lol-workshop.anaphoric-macros)
  
;; Many of these code examples are from Let over Lambda Chapter 6
;; and On Lisp Ch. 14

;; Basic Example: Suppose we have a function that computes a huge calculation
(defun some-long-calculation ()
  (loop :for i :from 1 :to 1000000000 :when i :sum :it))

;; in real code we would assign this to a variable and 
;;  (let ((result (some-long-calculation)))
;;    (foo result))

;; (aif (some-long-calculation)
;;      (foo it))

;; Graham's aif (canonical example)
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; Graham's awhen
(defmacro awhen (test-form &body body)
p  `(aif ,test-form
        (progn ,@body)))

(defmacro aunless (test-form &body body)
  `(aif (not ,test-form)
        (progn ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(awhen ,(car args) (aand ,@(cdr args))))))

;; (if (aand (owner it) (address it) (town it))
;;   (some-long-calculation))

;; Graham's alambda and ablock
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; example usage
(mapcar (alambda (n)
                 (if (> n 0)
                     (cons
                      n
                      (self (- n 1)))))
        (list 1 2 3 4 5))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                        (case (length args)
                          (0 nil)
                          (1 (car args))
                          (t `(let ((it ,(car args)))
                                ,(self (cdr args))))))
               args)))

;; Example expansion
;; (sb-cltl2:macroexpand-all
;;  '(ablock north-pole
;;    (princ "ho ")
;;    (princ it)
;;    (princ it)
;;    (return-from north-pole)))

;; sharp backquote
;; (defun mkstr (&rest args) (with-output-to-string (s)
;;                             (dolist (a args) (princ a s))))
;; (defun symb (&rest args)
;;   (values (intern (apply #'mkstr args))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun |#`-reader| (stream sub-char numarg)
;;     (declare (ignore sub-char))
;;     (unless numarg (setq numarg 1))
;;     `(lambda ,(loop for i from 1 to numarg
;; 		    collect (alexandria:symbolicate 'a (write-to-string i)))
;;        ,(funcall
;; 	 (get-macro-character #\`) stream nil)))
;;   (named-readtables:defreadtable :anaphoric-reader-macros
;;     (:merge :common-lisp)
;;     (:dispatch-macro-char #\# #\` #'|#`-reader|)
;;     (:case :preserve)))

;; (named-readtables:in-readtable :anaphoric-reader-macros)

;; ;; (set-dispatch-macro-character
;;  ;; #\# #\` #'|#`-reader|)

;; '#`((,a1))

;; ;; use numarg parameter of read macro
;; '#2`(,a1 ,a2)

;; ;; example of initializing using new reader macro
;; (let ((vars '(var-a var-b var-c)))
;;   (mapcar #2`(,a1 ,a2)
;;           vars
;;           (loop :for var :in vars
;;                 :collect (gensym (symbol-name var)))))

;; expansion code
(let ((vars '(var-a var-b var-c)))
    (mapcar (lambda (a1 a2) `(,a1 ,a2))
         vars
         (loop :for var :in vars
               :collect (gensym (symbol-name var)))))

;; serapeum example of above code with op
(let ((vars '(var-a var-b var-c)))
  (mapcar (serapeum:op `(,_1 ,_2))
          vars
          (loop :for var :in vars
                :collect (gensym (symbol-name var)))))

;; this example leverages 
(let ((vars '(var-a var-b var-c)))
  (mapcar (serapeum:op `(,_ ,_))
          vars
          (loop :for var :in vars
                :collect (gensym (symbol-name var)))))


(defmacro alet (letargs &body body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

;; Indirection chaining
(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setf this ,@(last body))
     ,@(butlast body)
     this))

;; alet% over dlambda
(defmacro dlambda (&rest ds)
  (alexandria:with-gensyms (args)
    `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar
            (lambda (d)
              `(,(if (eq t (car d))
                     t
                     (list (car d)))
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                            args
                            `(cdr ,args)))))
            ds)))))


;; alet over dlambda
(alet% ((sum) (mul) (expt))
       (funcall this :reset)
       (dlambda
        (:reset ()
                (psetf sum 0
                       mul 1
                       expt 2))
        (t (n)
           (psetf sum (+ sum n)
                  mul (* mul n)
                  expt (expt expt n))
           (list sum mul expt))))

(named-readtables:in-readtable :standard)
