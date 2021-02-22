;;;; test/def.lisp

(in-package #:galois-fields-test)

(deffield gf2 :p 2)
(deffield gf3 :p 3)

(nst:def-test-group deffield-tests ()
  (nst:def-test gf2-addition-function (:values (:equal 0) (:equal 1)
                                               (:equal 1) (:equal 0))
    (values (gf2+ 0 0) (gf2+ 0 1)
            (gf2+ 1 0) (gf2+ 1 1)))

  (nst:def-test gf2-multiplication-function (:values (:equal 0) (:equal 0)
                                                     (:equal 0) (:equal 1))
    (values (gf2* 0 0) (gf2* 0 1)
            (gf2* 1 0) (gf2* 1 1)))

  (nst:def-test gf3-addition-function (:values (:equal 0) (:equal 1) (:equal 2)
                                               (:equal 1) (:equal 2) (:equal 0)
                                               (:equal 2) (:equal 0) (:equal 1))
    (values (gf3+ 0 0) (gf3+ 0 1) (gf3+ 0 2)
            (gf3+ 1 0) (gf3+ 1 1) (gf3+ 1 2)
            (gf3+ 2 0) (gf3+ 2 1) (gf3+ 2 2))))
