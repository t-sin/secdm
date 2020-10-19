(defpackage #:secdm
  (:use #:cl
        #:secdm.vm
        #:secdm.compile)
  (:export #:make-vm
           #:run
           #:print-vm
           #:compile-lisp-1
           #:compile-lisp))
(in-package #:secdm)
