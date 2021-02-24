;; Copyright (c) 2021 nklein software
;; UNLICENSE License. See included UNLICENSE.txt file for licensing details.

(asdf:defsystem :galois-fields-test
  :description "tests for galois fields library"
  :version "0.1.20210221"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "UNLICENSE"
  :encoding :utf-8
  :depends-on ((:version #:galois-fields "0.1.20210221")
               #:nst)
  :components ((:static-file "UNLICENSE.txt")
               (:module "test"
                :components ((:file "package")
                             (:file "def" :depends-on ("package"))
                             (:file "run" :depends-on ("package"))))))
