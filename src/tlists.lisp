(defpackage :afp.lol.tlists
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum)))

(in-package :afp.lol.tlists)

(declaim (inline make-tlist tlist-left tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))

(defun tlist-left (tl) (caar tl))

(defun tlist-right (tl) (cadr tl))

(defun tlist-empty-p (tl) (null (car tl)))
