(defpackage #:secdm.compile
  (:use #:cl)
  (:export #:compile-lisp-1
           #:compile-lisp))
(in-package #:secdm.compile)

(defun find-variable (v code)
  (if (null code)
      nil
      (let ((bound-p nil))
        (dolist (c (rest code))
          (setf bound-p (or bound-p
                            (typecase c
                              (symbol (eq v c))
                              (list (find-variable v c))
                              (t nil)))))
        bound-p)))

(defstruct compiler-state
  (compiling-name nil)
  (name-table (make-hash-table)))

(defun compile-lambda (args body)
  "0-arity function is ether a constant or process that has side effect."
  ;; maybe wip
  (if (null args)
      (compile-lisp-1 body)
      `((ldf ,(compile-lambda (cdr args) body)))))

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
                       (eq)))
                ;; special forms
                (:quote `((ldc ,(second code))))
                (:if (let ((cond (second code))
                           (true (third code))
                           (false (fourth code)))
                       `(,@(compile-lisp-1 cond)
                         (sel (,@(compile-lisp-1 true) (join))
                              (,@(compile-lisp-1 false) (join))))))
                (:lambda (compile-lambda (second code) (third code)))))))
    (t `((ldc ,code)))))

(defun compile-lisp (code-list)
  "Compile multiple Lisp code to SECD machine code."
  (loop
    :for code :in code-list
    :append (compile-lisp-1 code)))
