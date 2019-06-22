(defpackage :afp-lol-workshop.pandoric-macros
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:nicknames "pandoric-macros"))

(in-package :afp-lol-workshop.pandoric-macros)

;; motivation: use of with-slots
(defclass student ()
  ((name :accessor name :initarg :name)
   (age :accessor age :initarg :age)
   (year-in-school :reader year-in-school :initarg :year-in-school)))

(defmethod update-year-in-school ((o student) new-year)
  (with-slots (year-in-school) o
    (setf year-in-school new-year)))


(defun mkstr (&rest args) (with-output-to-string (s)
                            (dolist (a args) (princ a s))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

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

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)


(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error
          "Unknown pandoric get: ~a"
          sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                   (setq ,(car a1) val))
               letargs)
     (t (error
          "Unknown pandoric set: ~a"
          sym val))))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons
                   '(this)
                   (let-binding-transform
                     letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda 
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
           (apply this args))))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))


;; with-pandoric
(a:with-gensyms (obox)
  `(defmacro with-pandoric (syms ,obox &body body)
    (a:with-gensyms (gbox)
      `(symbol-macrolet
	   (,@(mapcar #`(,a1 (get-pandoric ,gbox ',a1))
		      syms))
	 ,@body))))


;; Example of everything put together

(setf (symbol-function 'pantest)
    (pandoriclet ((acc 0))
      (lambda (n) (incf acc n))))
