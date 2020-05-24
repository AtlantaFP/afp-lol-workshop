(defpackage :afp.lol.pointer-scope
  (:use :cl))

(in-package :afp.lol.pointer-scope)

;; a macro that will simulating getting a pointer reference
(defmacro pointer-& (obj)
  (a:with-gensyms (set temp)
    `(lambda (&optional (,set ',temp))
       (if (eq ,set ',temp)
           ,obj
           (setf ,obj ,set)))))

(defun pointer-* (addr)
  (funcall addr))

(defsetf pointer-* (addr) (val)
  `(funcall ,addr ,val))

(defsetf pointer-& (addr) (val)
  `(setf (pointer-* ,addr) ,val))

;; example usage
(let ((x 0))
  (pointer-& (pointer-& x)))

(defparameter *temp-pointer* *)

(disassemble (lambda (arr ind)
               (aref arr ind)))

(disassemble (lambda (arr ind)
               (declare ((simple-array fixnum) arr) (fixnum ind))
               (aref arr ind)))

(disassemble (lambda (arr ind)
               (declare ((simple-array fixnum) arr) (fixnum ind) (optimize (speed 3) (safety 0)))
               (aref arr ind)))

(defmacro with-fast-stack
    ((sym &key (type 'fixnum) (size 1000) (safe-zone 100))
     &body body)
  (a:with-gensyms (index mem)
    `(let ((,index ,safe-zone)
           (,mem (make-array ,(+ size (* 2 safe-zone))
                             :element-type ',type)))
       (declare (type (simple-array ,type) ,mem)
                (type fixnum ,index))
       (macrolet
           ((,(a:symbolicate 'fast-push- sym) (val)
              `(locally
                   (declare (optimize (speed 3) (safety 0)))
                 (setf (aref ,',mem ,',index) ,val)
                 (incf ,',index)))
            (,(a:symbolicate 'fast-pop- sym) ()
              `(locally
                   (declare (optimize (speed 3) (safety 0)))
                 (decf ,',index)
                 (aref ,',mem ,',index)))
            (,(a:symbolicate 'check-stack- sym) ()
              `(progn
                 (if (<= ,',index ,,safe-zone)
                     (error "Stack underflow: ~a" ',',sym))
                 (if (<= ,,(- size safe-zone) ,',index)
                     (error "Stack overflow: ~a" ',',sym)))))
         ,@body))))

(time ((lambda (a)
         (declare (fixnum a))
         (with-fast-stack (input :size 2000)
           (loop :repeat 1000 :do
             (fast-push-input a)))) 5)))

