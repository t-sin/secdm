(defpackage #:secdm.compile
  (:use #:cl)
  (:export #:compile-lisp-1
           #:compile-lisp))
(in-package #:secdm.compile)

(defun compile-lisp-1 (code)
  "Compile Lisp code to SECD machine code."
  (typecase code
    (t (list 'ldc code))))

(defun compile-lisp (code-list)
  "Compile multiple Lisp code to SECD machine code."
  (loop
    :for code :in code-list
    :collect (compile-lisp-1 code)))
