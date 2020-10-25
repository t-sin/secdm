(defpackage #:secdm.compile
  (:use #:cl)
  (:export #:compile-lisp-1
           #:compile-lisp))
(in-package #:secdm.compile)

(defun replace-arg (arg n code)
  "Replace all symbol named `arg` with `n`.

  It's used to embed argument marker (see `compile-lambda`)."
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

(defun compile-lambda (args body state &optional (argnum (1- (length args))))
  "Compile lambda expression to SECD machine code.

  Because functions in SECD machine are curried, so multiple number of arguments are
  compiled into nested 1-arity functions. But SECD machine clears S stack when `ap` or `rap`,
  so we cannot pass actual arguments over functional application. Here, this function
  embeds argument marker (it is just an integer) into the place of actual argument.

  Note that 0-arity function is ether a constant or process that has side effect but
  there is no 0-arity function."
  (if (null args)
      (compile-lisp-1 body state)
      (let ((args* (cdr args))
            (body* (replace-arg (car args) argnum body)))
        (let ((inner (compile-lambda args* body* state (1- argnum))))
          ;; argnum should be replaced with actual argument code when function application
          `((ldf (,@(unless (zerop argnum) (list argnum)) ,@inner (rtn))) (ap))))))

(defun compile-apply (args code state)
  "Compile function application.

  This function replaces argument marker with actual actument expression."
  (if (null args)
      code
      (let* ((op (first code))
             (op-name (first op))
             (op-body (second op)))
        (if (and (eq op-name 'ldf) (numberp (first op-body)))
            `((ldf (,@(compile-lisp-1 (elt args 0) state)
                    ,@(compile-apply (cdr args) (rest op-body) state)))
              ,@(rest code))
            code))))

(defun compile-lisp-1 (code state)
  "Compile Lisp code to SECD machine code."
  (typecase code
    (list (let ((op (car code)))
            (assert (symbolp op))
            (let ((name (intern (symbol-name op) :keyword)))
              (case name
                ;; 0-arity functon
                (:nil `((ldc nil)))
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
                     (let ((vcode (gethash name (compiler-state-name-table state))))
                       (when (null vcode)
                         (error "undefined name `~s`." name))
                       (let ((fncode (compile-apply (rest args) vcode state)))
                       `(,@(compile-lisp-1 (first args) state)
                         ,@fncode)))))))))
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
