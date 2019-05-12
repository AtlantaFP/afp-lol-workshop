(defpackage afp-lol.reader-macros-examples
  (:use :cl)
  (:nicknames "reader-macros-examples"))
(in-package :afp-lol.reader-macros-examples)
#|
What are reader macros?
- macros that take the stream of text (at read time) and then perform transformations
on the actual stream of text and return Lisp Objects

|#

;; example of showing off what is actually backquote forms read
(let (*print-pretty*)
  (print
   '`(football-game
      (game-started-at ,(get-internal-real-time))
      (coin-flip
       (if (zerop (random 2))
           'heads
           'tails)))))


#|
a first example of read macro. #> (mimics Perl's << operator) takes
a stream of text and returns a proper Lisp string with special characters
properly escaped
|#

;; first define a function that implements the processing
;; original version
(defun |#>-reader| (stream subchar numarg)
  (declare (ignore subchar numarg))
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
           (pointer pattern)
           (output))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((null pointer))
        (push curr output)
        (setf pointer
              (if (char= (car pointer) curr)
                  (cdr pointer)
                  pattern))
        (if (null pointer)
            (return)))
      (coerce
       (nreverse (nthcdr (length pattern) output))
       'string))))

(set-dispatch-macro-character #\# #\> #'|#>-reader|)

#>END
This is a "test"...
END

;; version from the production repo ()
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= #\newline curr))
        (push curr chars))
      (let ((pattern (nreverse chars))
            output)
        (labels ((match (pos chars)
                   (if (null chars)
                       pos
                       (if (char= (nth pos pattern) (car chars))
                           (match (1+ pos) (cdr chars))
                           (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
          (do (curr
               (pos 0))
              ((= pos (length pattern)))
            (setf curr (read-char stream)
                  pos (match pos (list curr)))
            (push curr output))
          (coerce
           (nreverse
            (nthcdr (length pattern) output))
           'string))))))

;; now notify the reader that upon seeing the character sequence #> use this function
(set-dispatch-macro-character #\# #\> #'|#>-reader|)

;; TODO: Need to update to read subexpressions
;; take read-delimited-list
(defun |#[-reader| (stream subchar)
  (declare (ignore subchar))
  (loop :for curr = (read-char stream)
        :until (char= #\] curr)
        :do (print curr)))

;; WARNING: if you uncomment this and run it, you will no longer be able use your REPL
;; EVAL AT YOUR OWN RISK!
;; (set-macro-character #\( (lambda (stream ch)
;;                           (declare (ignore ch))
;;                           (read-delimited-list #\) stream)
;;                           (values)))

(defparameter *not-shared* (list (list 1) (list 1)))
(print (eq (car *not-shared*) (cadr *not-shared*)))

(defparameter *shared* (list #0='(1) #0#))
