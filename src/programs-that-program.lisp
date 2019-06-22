(defpackage :programs-that-program
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum)))

(in-package :programs-that-program)

(defun adder (x)
  (+ x 1))

;; example of defining macro for units of measure
(defmacro defunits% (quantity base-unit &body units)
  (a:with-gensyms (val un)
     `(defmacro ,(a:symbolicate 'unit-of- quantity) (,val ,un)
       `(* ,,val
           ,(case ,un
              (,base-unit 1)
              ,@(mapcar (lambda (x)
                          `(,(car x) ,(cadr x)))
                 (s:batches units 2)))))))

(defun %defunits-chaining (u units prev)
  (when (member u prev)
    (error "~{ ~a~^ depends on~}" (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if spec
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (%defunits-chaining
                  (cadr chain)
                  units
                  (cons u prev)))
              chain))
        (error "Unknown unit: ~a" u))))

(defmacro defunits (quantity base-unit &body units)
  `(defmacro ,(a:symbolicate 'unit-of- quantity) (value unit)
     `(* ,value
         ,(case unit
            ((,base-unit) 1)
            ,@(mapcar
               (lambda (x)
                 `((,(car x))
                   ,(%defunits-chaining
                     (car x)
                     (cons
                      `(,base-unit 1)
                      (s:batches units 2))
                     nil)))
               (s:batches units 2))))))

;; (defunits time s
;;   m 60
;;   h (60 m)
;;   d (24 h)
;;   ms (1/1000 s)
;;   us (1/1000 ms))

;; (defunits time s
;;   m (1/60 h)
;;   h (60 m))

;;(unit-of-time 1 d) ;; (* 24 (* 60 (* 60 1)))

(defun tree-leaves% (tree result)
  (when tree
      (if (listp tree)
          (cons
           (tree-leaves% (car tree)
                         result)
           (tree-leaves% (cdr tree)
                         result))
          result)))

(sort '(5 1 2 4 3) #'<)

(defun tree-leaves%% (tree test result)
  (when tree
    (if (listp tree)
        (cons (tree-leaves%% (car tree) test result)
              (tree-leaves%% (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
            tree))))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%% ,tree
                  (lambda (it)
                    (declare (ignorable it))
                    ,test)
                  (lambda (it)
                    (declare (ignorable it))
                    ,result)))
(print (tree-leaves '(2 (nil t (a . b)))
                    (lambda (x)
                      (and (numberp x) (evenp x)))
                    (lambda (x)
                      (* x x))))


(print (tree-leaves '(2 (nil t (a . b)))
              (and (numberp it) (evenp it))
              'even-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macrolet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; original version of nlet-tail
(defmacro nlet (n letargs &body body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

;; our version of tail recursive version of nlet-fact
(defun nlet-fact-tr (n)
  (nlet fact ((n n)
              (acc 1))
    (if (zerop n)
        acc
        (fact (- n 1) (* acc n)))))

;; tail recursive version of nlet macro that leverages macrolet
(defmacro nlet-tail (n letargs &body body)
  (a:with-gensyms (nn b)
    (let ((gs (loop :for i
                    :in letargs
                    :collect (gensym))))
      `(macrolet
           ((,n ,gs
              `(progn
                 (psetf
                  ,@(apply #'nconc
                           (mapcar
                            #'list
                            ',(mapcar #'car letargs)
                            (list ,@gs))))
                 (go ,',nn))))
         (block ,b
           (let ,letargs
             (tagbody
                ,nn (return-from ,b (progn ,@body)))))))))

;; example usage
(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
    (if (zerop n)
        acc
        (fact (- n 1) (* acc n)))))

;; (macrolet ((fact (g745 g746)
;;              `(progn
;;                 (psetf ,@(apply #'nconc
;;                                 (mapcar #'list '(n acc) (list g745 g746))))
;;                 (go ,'nn743))))
;;   (block b744
;;     (let ((n n) (acc 1))
;;       (tagbody
;;          nn743
;;          (return-from b744
;;            (progn
;;              (if (zerop n)
;;                  acc
;;                  (fact (- n 1) (* acc n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recursive Expansions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cxr% (x tree)
  (if (null x)
      tree
      `(,(cond
	   ((eq 'a (cadr x)) 'car)
	   ((eq 'd (cadr x)) 'cdr)
	   (t (error "Non A/D symbol")))
	,(if (= 1 (car x))
	     `(cxr% ,(cddr x) ,tree)
	     `(cxr% ,(cons (- (car x) 1) (cdr x))
		    ,tree)))))

;; example usage
(defun eleventh (x)
  (cxr% (1 a 10 d) x))

(defparameter cxr-inline-thresh 10)

(defmacro cxr (x tree)
  (a:with-gensyms (name val count)
    (if (null x)
	tree
	(let ((op (cond
		    ((eq 'a (cadr x)) 'car)
		    ((eq 'd (cadr x)) 'cdr)
		    (t (error "Non A/D symbol")))))
	  (if (and (integerp (car x))
		   (<= 1 (car x) cxr-inline-thresh))
	      (if (= 1 (car x))
		  `(,op (cxr ,(cddr x) ,tree))
		  `(,op (cxr ,(cons (- (car x) 1) (cdr x))
			     ,tree)))
	      `(nlet-tail
		   ,name ((,count ,(car x))
			  (,val (cxr ,(cddr x) ,tree)))
		 (if (>= 0 ,count)
		     ,val
		     (,name (- ,count 1)
			    (,op ,val)))))))))

;; (defun nthcdr% (n list)
;;   (cxr (n d) list))

;; (defun nth% (n list)
;;   (cxr (1 a n d) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recursive Solutions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-english-list-accessors (start end)
  (when (not (<= 1 start end))
    (error "Bad start/end range"))
  `(progn
     ,@(loop :for i
	     :from start
	       :to end
	     :collect
	     `(defun
		  ,(a:symbolicate
		    (map 'string
			 (lambda (c)
			   (if (alpha-char-p c)
			       (char-upcase c)
			       #\-))
			 (format nil "~:r" i)))
		  (arg)
		(cxr (1 a ,(- i 1) d) arg)))))


(defun cxr-symbol-p (s)
  (if (symbolp s)
      (let ((chars (coerce
		    (symbol-name s)
		    'list)))
	(and
	 (< 6 (length chars))
	 (char= #\C (car chars))
	 (char= #\R (car (last chars)))
	 (null (remove-if
		(lambda (c)
		  (or (char= c #\A)
		      (char= c #\D)))
		(cdr (butlast chars))))))))

(defun cxr-symbol-to-cxr-list (s)
  (labels ((collect (l)
	     (if l
		 (list*  1
			 (if (char= (car l) #\A)
			     'A
			     'D)
			 (collect (cdr l))))))
    (collect
	(cdr      ; chop off c
	 (butlast ; chop off R
	  (coerce
	   (symbol-name s)
	   'list))))))

(defmacro with-all-cxrs (&rest forms)
  `(labels
       (,@(mapcar
	    (lambda (s)
	      `(,s (l)
		   (cxr ,(cxr-symbol-to-cxr-list s)
			l)))
	    (remove-duplicates
	     (remove-if-not
	      #'cxr-symbol-p
	      (a:flatten forms)))))))

;; (with-all-cxrs #'cadadadadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dlambda
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-counter* (let ((count 0))
   (lambda (msg)
     (case msg
       (:inc
        (incf count))
       (:dec
        (decf count))))))

(defmacro dlambda (&rest ds)
  (a:with-gensyms (args)
    `(lambda (&rest ,args)
       (case (car ,args)
	 ,@(mapcar
	    (lambda (d)
	      `(,(if (eq t (car d))
		     t
		     (car d))
		(apply (lambda ,@(cdr d))
		       ,(if (eq t (car d))
			    args
			    `(cdr ,args)))))
	    ds)))))

;; example usage of dlambda: creating a counter with multiple methods
(setf (symbol-function 'count-test)
      (let ((count 0))
	(dlambda
	 (:inc () (incf count))
	 (:dec () (decf count))
	 (:reset () (setf count 0)))))

