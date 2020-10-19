(defpackage #:secdm
  (:use #:cl
        #:secdm.vm)
  (:export #:make-vm
           #:run
           #:print-vm))
(in-package #:secdm)
