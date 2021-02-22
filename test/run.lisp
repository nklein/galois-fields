;;;; test/run.lisp

(in-package #:galois-fields-test)

(defun run-tests (&key
                    (debug-on-error nst:*debug-on-error*)
                    (debug-on-fail nst:*debug-on-fail*))
  (let ((nst:*debug-on-error* debug-on-error)
        (nst:*debug-on-fail* debug-on-fail)
        (*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)))
