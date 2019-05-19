(defpackage :afp-lol-workshop.anaphoric-macros
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:nicknames "anaphoric-macros"))
(in-package :afp-lol-workshop.anaphoric-macros)

;; Many of these code examples are from Let over Lambda Chapter 6

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; example usage

(alambda (n)
         (if (> n 0)
             (cons
              n
              (self (- n 1)))))

;; Graham's aif
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


;; sharp backquote
(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\` #'|#`-reader|)

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

;; alet% over dlambda
(defmacro dlambda (&rest ds)
  (a:with-gensyms (args)
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


(alet% ((sum) (mul) (expt))
       (funcall this :reset)
       (dlambda
        (:reset ()
                (psetq sum 0
                       mul 1
                       expt 2))
        (t (n)
           (psetq sum (+ sum n)
                  mul (* mul n)
                  expt (expt expt n))
           (list sum mul expt))))
