(defpackage #:secdm.compile
  (:use #:cl)
  (:export #:compile-lisp-1
           #:compile-lisp))
(in-package #:secdm.compile)

(defun compile-lisp-1 (code)
  "Compile Lisp code to SECD machine code."
  (typecase code
    (list (let ((op (car code)))
            (assert (symbolp op))
            (let ((name (intern (symbol-name op) :keyword)))
              (case name
                (:atom `(,@(compile-lisp-1 (cadr code)) (atom)))))))
    (t `((ldc ,code)))))

(defun compile-lisp (code-list)
  "Compile multiple Lisp code to SECD machine code."
  (loop
    :for code :in code-list
    :append (compile-lisp-1 code)))
