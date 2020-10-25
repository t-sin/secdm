(defpackage #:secdm.compile
  (:use #:cl)
  (:export #:compile-lisp-1
           #:compile-lisp))
(in-package #:secdm.compile)

(defun replace-arg (arg n code)
  (if (null code)
      code
      (if (listp code)
          (loop
            :for c :in code
            :collect (if (listp c)
                         (replace-arg arg n c)
                         (if (eq c arg)
                             (intern (format nil "~a" n) :keyword)
                             c)))
          code)))

(defstruct compiler-state
  (compiling-name nil)
  (name-table (make-hash-table)))

(defun compile-lambda (args body state &optional (nest 0))
  "0-arity function is ether a constant or process that has side effect."
  ;; maybe wip
  (if (null args)
      (compile-lisp-1 body state)
      (let ((args* (cdr args))
            (body* (replace-arg (car args) nest body)))
        `((ldf (,@(compile-lambda args* body* state (1+ nest)) (rtn)))))))

(defun compile-lisp-1 (code state)
  "Compile Lisp code to SECD machine code."
  (typecase code
    (list (let ((op (car code)))
            (assert (symbolp op))
            (let ((name (intern (symbol-name op) :keyword)))
              (case name
                ;; 1-arity functions
                (:atom `(,@(compile-lisp-1 (second code) state) (atom)))
                (:car `(,@(compile-lisp-1 (second code) state) (car)))
                (:cdr `(,@(compile-lisp-1 (second code) state) (cdr)))
                ;; 2-arity functions
                (:cons `(,@(compile-lisp-1 (second code) state)
                         ,@(compile-lisp-1 (third code) state)
                         (cons)))
                (:eq `(,@(compile-lisp-1 (second code) state)
                       ,@(compile-lisp-1 (third code) state)
                       (eq)))
                ;; 2-arity arithmetic functions
                ;; special forms
                (:quote `((ldc ,(second code))))
                (:if (let ((cond (second code))
                           (true (third code))
                           (false (fourth code)))
                       `(,@(compile-lisp-1 cond state)
                         (sel (,@(compile-lisp-1 true state) (join))
                              (,@(compile-lisp-1 false state) (join))))))
                (:lambda (compile-lambda (second code) (third code) state))
                (:define (let ((name (second code))
                               (body (third code)))
                           (if (symbolp name)
                               (progn
                                 (setf (compiler-state-compiling-name state) name)
                                 (setf (gethash name (compiler-state-name-table state))
                                       (compile-lisp-1 body state))
                                 (setf (compiler-state-compiling-name state) nil))
                               (error "`~s` is not a symbol." name))))
                ;; function applications
                (t (let ((name (first code))
                         (args (rest code)))
                     `(,@(loop
                           :for a :in args
                           :append (compile-lisp-1 a state))
                       ,@(gethash name (compiler-state-name-table state))
                       (ap))))))))
    (keyword `((ld ,(parse-integer (symbol-name code)))))
    (symbol (let ((val (gethash code (compiler-state-name-table state))))
              (if val
                  val
                  (error "undefined name: `~s`." code))))
    (t `((ldc ,code)))))

(defun compile-lisp (code-list)
  "Compile multiple Lisp code to SECD machine code."
  (let ((state (make-compiler-state)))
    (loop
      :for code :in code-list
      :append (compile-lisp-1 code state))))
