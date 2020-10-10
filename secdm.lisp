(defpackage #:secdm
  (:use #:cl)
  (:export))
(in-package #:secdm)

(defstruct env
  type parent table)

(defstruct vm
  s e c d)
