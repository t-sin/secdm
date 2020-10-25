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

(defun compile-lambda-1 (args arglen body state)
  "Compile lambda expression to SECD machine code.

  Because functions in SECD machine are curried, so multiple number of arguments are
  compiled into nested 1-arity functions. But SECD machine clears S stack when `ap` or `rap`,
  so we cannot pass actual arguments over functional application. Here, this function
  embeds argument marker (it is just an integer) into the place of actual argument.

  Note that 0-arity function is ether a constant or process that has side effect but
  there is no 0-arity function."
  (if (null args)
      (compile-lisp-1 body state)
      (let* ((arglen* (length args))
             (args* (cdr args))
             (body* (replace-arg (car args) (- arglen arglen*) body)))
        (let ((inner (compile-lambda-1 args* arglen body* state))
              (arg-exist-p (> arglen* 1)))
          ;; argnum should be replaced with actual argument code when function application
          (if arg-exist-p
              `((ldf (,arglen* ,@inner (ap) (rtn))))
              `((ldf (,@inner (rtn)))))))))

(defun compile-lambda (args body state)
  "compile lambda expression to secd machine code.

  this function compiles starter function to recursive function call."
  (let ((rec-body (replace-arg (compiler-state-compiling-name state) 0 body))
        (arglen (length args)))
    (compile-lambda-1 args arglen rec-body state)))

(defun compile-apply-1 (args code state)
  (if (null args)
      code
      (let* ((op (first code))
             (op-name (first op))
             (op-body (second op)))
        (if (and (eq op-name 'ldf) (numberp (first op-body)))
            `((ldf (,@(compile-lisp-1 (first args) state)
                    ,@(compile-apply-1 (cdr args) (rest op-body) state)))
              ,@(rest code))
            code))))

(defun compile-apply (args code state)
  "compile function application.

  this function replaces argument marker with actual actument expression."
  `((dum)
    ,@(compile-apply-1 (rest args) code state)
    (ldf (,@(compile-lisp-1 (first args) state) (ld 0) (ap) (rtn)))
    (rap)))

(defun compile-lisp-1 (code state)
  "Compile Lisp code to SECD machine code."
  (typecase code
    (list (let ((op (car code)))
            (assert (symbolp op))
            (let ((name (intern (symbol-name op) :keyword)))
              (case name
                ;; 0-arity functon
                (:nil `(secdm.op::nil))
                ;; 1-arity functions
                (:atom `(,@(compile-lisp-1 (second code) state) (atom)))
                (:car `(,@(compile-lisp-1 (second code) state) (car)))
                (:cdr `(,@(compile-lisp-1 (second code) state) (cdr)))
                ;; 2-arity functions
                (:cons`(,@(compile-lisp-1 (second code) state)
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
                       `(,@(compile-apply args vcode state)))))))))
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
