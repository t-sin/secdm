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
        ((consp code) (cons (let ((car (car code)))
                              (if (null car)
                                  (ensure-secdm-package :nil)
                                  (ensure-secdm-package car)))
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

(defop nil (vm)
    "Load nil to S register."
  (push nil (vm-s vm)))

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
    (let ((dump (list :s (vm-s vm) :e (vm-e vm) :c (vm-c vm)))
          (env (append (list v) (vm-e vm))))
      (setf (vm-s vm) nil
            (vm-e vm) env
            (vm-c vm) (getf (cdr f) :code))
      (push dump (vm-d vm)))))

(defop rtn (vm)
    "Exit from function evaluation and return a value on the top of S stack."
  (let ((v (pop (vm-s vm)))
        (dump (pop (vm-d vm))))
    (setf (vm-s vm) (append (list v) (getf dump :s))
          (vm-e vm) (getf dump :e)
          (vm-c vm) (getf dump :c))))

(defop dum (vm)
    "Load a dummy value to current enviroment.

    This dummy value :omega will be replaced by `rap` operator."
  (push :omega (vm-e vm)))

(defop rap (vm)
    "Apply a function recursively.

    This operator assumes that the function `f` has :omega in front of its environment.
    So before `ldf` user should call `dum`."
  (let ((f (pop (vm-s vm)))
        (v (pop (vm-s vm))))
    (assert (eq (car f) :fn))
    (let ((dump (list :s (vm-s vm)
                      :e (cdr (vm-e vm))  ; dumps :omega removed environment
                      :c (vm-c vm)))
          (env (getf (cdr f) :env)))
      (rplaca env v)  ; replace :omega
      (setf (vm-s vm) nil
            (vm-e vm) env
            (vm-c vm) (getf (cdr f) :code))
      (push dump (vm-d vm)))))

(defop sel (vm true false)
    "Conditional branching in SECD machine.

    This operator takes two arguments true clause and false clause.
    If the value of top of S stack is true the machine executes true clause,
    otherwize the machine executes false one.
    Each clause should be ended with `join`, the brancing termination operator."
  (let ((v (pop (vm-s vm)))
        (dump (list :s nil :e nil :c (vm-c vm))))
    (push dump (vm-d vm))
    (setf (vm-c vm) (if v true false))))

(defop join (vm)
    "Terminate branching.

    True/False causes for `sel` should ends with this operator."
  (let ((dump (pop (vm-d vm))))
    (setf (vm-c vm) (getf dump :c))))

