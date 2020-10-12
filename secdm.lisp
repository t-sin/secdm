(defpackage #:secdm
  (:use #:cl)
  (:export))
(in-package #:secdm)

(defstruct (vm (:constructor make-vm*))
  (debug-p nil)
  (step-p nil)
  s e c d)

(defmethod print-object ((vm vm) stream)
  (format stream
          "VM:~%  S: ~s~%  E: ~s~%  C: ~s~%  D: ~s~%"
          (vm-s vm) (vm-e vm) (vm-c vm) (vm-d vm)))

(defun run-1 (vm)
  "Run one step of evaluation.

  Each operators are given as a list like `(ldc 1)`."
  (when (vm-debug-p vm)
    (format t "~s" vm))
  (when (null (vm-c vm))
    (return-from run-1 nil))
  (let ((op (pop (vm-c vm))))
    (assert (listp op))
    (let ((name (car op))
          (args (cdr op)))
      (assert (symbolp name))
      (apply name vm args)
      t)))

(defun run (vm)
  (loop
    (unless (run-1 vm)
      (return-from run))
    (when (vm-step-p vm)
      (if (or (format t "continue? (y/n): ")
              (eq (read *standard-input*) 'y))
          nil
          (return-from run)))))

(defpackage #:secdm/op
  (:use))

(defpackage #:secdm/symbol
  (:use #:secdm/op))

(defun ensure-secdm-package (code)
  (cond ((or (null code)
             (numberp code))
         code)
        ((symbolp code) (intern (symbol-name code) :secdm/symbol))
        ((consp code) (cons (ensure-secdm-package (car code))
                            (ensure-secdm-package (cdr code))))))

(defun make-vm (debug-p code)
  (let ((code (ensure-secdm-package code)))
    (make-vm* :c code :debug-p debug-p)))

(defmacro defop (name (&rest args) doc &body body)
  (let ((fn-name (intern (symbol-name name) :secdm/op)))
    `(progn
       (defun ,fn-name ,args
         ,doc ,@body)
       (export ',fn-name :secdm/op))))

(defop ldc (vm n)
    "Load a constant to S register."
  (push n (vm-s vm)))

(defop ld (vm idx)
    "Load a value with `idx` in current envionment to S register."
  (let ((v (elt (vm-e vm) idx)))
    (push v (vm-s vm))))

(defop ldf (vm code)
    "Load a function to S register."
  (let ((f (list :fn :code code :env (vm-e vm))))
    (push f (vm-s vm))))

(defop ap (vm)
    "Apply a function to a value on top of S register."
  (let ((f (pop (vm-s vm)))
        (v (pop (vm-s vm))))
    (assert (eq (car f) :fn))
    (let ((dump (list (vm-s vm) (vm-e vm) (vm-c vm)))
          (env (append (list v) (vm-e vm))))
      (setf (vm-s vm) nil
            (vm-e vm) env
            (vm-c vm) (getf (cdr f) :code))
      (push dump (vm-d vm)))))
