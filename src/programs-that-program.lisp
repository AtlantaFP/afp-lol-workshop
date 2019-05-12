(defpackage :programs-that-program
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum)))

(in-package :programs-that-program)

(defun adder (x)
  (+ x 1))

(defunits time s
  m 60
  h (60 m)
  d (24 h)
  ms (1/1000 s)
  us (1/1000 ms))

(defunits time s
  m (1/60 h)
  h (60 m))

(unit-of-time 1 d) ;; (* 24 (* 60 (* 60 1)))

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

(defun tree-leaves% (tree result)
  (when tree
      (if (listp tree)
          (cons
           (tree-leaves% (car tree)
                         result)
           (tree-leaves% (cdr tree)
                         result))
          result)))

(print (tree-leaves '(2 (nil t (a . b)))
                    (lambda (x)
                      (and (numberp x) (evenp x)))
                    (lambda (x)
                      (* x x))))


(print (tree-leaves '(2 (nil t (a . b)))
              (and (numberp it) (evenp it))
              'even-number))

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

;; tail recursive version of nlet macro
(defmacro nlet-tail (n letargs &body body)
  (a:with-gensyms (nn b)
    (let ((gs (loop :for i
                    :in letargs
                    :collect (gensym))))
      `(macrolet
           ((,n ,gs
              `(progn
                 (psetq
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
