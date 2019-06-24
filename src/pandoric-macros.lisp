(defpackage :afp-lol-workshop.pandoric-macros
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:nicknames "pandoric-macros"))

(in-package :afp-lol-workshop.pandoric-macros)

;; motivation: use of with-slots
(define-handy-method (double (o Point))
    (set! x (* 2 x))
  (set! y (* 2 y)))

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
          "Unknown pandoric set: ~a ~a"
          sym val))))

(defmacro pandoriclet (letargs &body body)
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
(defmacro with-pandoric (syms obox &body body)
  (a:once-only (obox)
    `(symbol-macrolet (,@(mapcar (s:op `(,_1 (get-pandoric ,obox ',_1)))
                                 syms))
       ,@body)))

(with-pandoric (acc) #'pantest
  (format t "Value of acc: ~a~%" acc))

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setf this new)))

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setf this ,new)))

;; Example of everything put together

(setf (symbol-function 'pantest)
    (pandoriclet ((acc 0))
      (lambda (n) (incf acc n))))

(pandoric-hotpatch #'pantest
                   (let ((acc 100))
                     (lambda (n) (decf acc n))))

(pandoric-recode (acc) #'pantest
                 (lambda (n) (decf acc n)))

(defmacro plambda (largs pargs &body body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setf
        this (lambda ,largs ,@body)
        self (dlambda
              (:pandoric-get (sym)
                             ,(pandoriclet-get pargs))
              (:pandoric-set (sym val)
                             ,(pandoriclet-set pargs))
              (t (&rest args)
                 (apply this args)))))))

(setf (symbol-function 'pantest)
      (let ((a 0) (b 1))
        (plambda (n) (a b)
          (incf a n)
          (setq b (* b n)))))

(setf (symbol-function 'pantest)
      (pandoriclet ((a 0) (b 1))
        (lambda (n)
          (incf a n)
          (setq b (* b n)))))

(defmacro defpan (name args &rest body)
  `(defun ,name (self)
     ,(if args
          `(with-pandoric ,args self
             ,@body)
          `(progn ,@body))))

;; stats counter example from Let over Lambda Ch. 6

(defpan stats-counter-mean (sum count)
  (/ sum count))

(defpan stats-counter-variance
    (sum-of-squares sum count)
  (if (< count 2)
      0
      (/ (- sum-of-squares
            (* sum
               (stats-counter-mean self)))
         (- count 1))))

(defpan stats-counter-stddev ()
  (sqrt (stats-counter-variance self)))

(defun make-noisy-stats-counter
    (&key (count 0)
       (sum 0)
       (sum-of-squares 0))
  (plambda (n) (sum count sum-of-squares)
    (incf sum-of-squares (expt n 2))
    (incf sum n)
    (incf count)

    (format t
            "~&MEAN=~a~%VAR=~a~%STDDEV=~a~%"
            (stats-counter-mean self)
            (stats-counter-variance self)
            (stats-counter-stddev self))))

(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
           (plambda () ,vars t)))
     (eval `(with-pandoric
                ,',vars ',pandoric-eval-tunnel
              ,,expr))))

(let ((x 1))
  (pandoric-eval (x)
                 '(+ 1 x)))
