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
                ;; 1-arity functions
                (:atom `(,@(compile-lisp-1 (second code)) (atom)))
                (:car `(,@(compile-lisp-1 (second code)) (car)))
                (:cdr `(,@(compile-lisp-1 (second code)) (cdr)))
                ;; 2-arity functions
                (:cons `(,@(compile-lisp-1 (second code))
                         ,@(compile-lisp-1 (third code))
                         (cons)))
                (:eq `(,@(compile-lisp-1 (second code))
                       ,@(compile-lisp-1 (third code))
                       (eq)))))))
    (t `((ldc ,code)))))

(defun compile-lisp (code-list)
  "Compile multiple Lisp code to SECD machine code."
  (loop
    :for code :in code-list
    :append (compile-lisp-1 code)))
