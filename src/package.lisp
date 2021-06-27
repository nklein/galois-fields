;;;; src/package.lisp

(defpackage #:galois-fields
  (:use #:cl)
  (:export #:deffield
           #:deffield*
           #:gf+
           #:gf*
           #:gf+-function
           #:gf*-function))
