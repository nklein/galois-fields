;; Copyright (c) 2021 nklein software
;; UNLICENSE License. See included UNLICENSE.txt file for licensing details.

(asdf:defsystem :galois-fields
  :description "galois fields library"
  :version "0.2.20210224"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "UNLICENSE"
  :encoding :utf-8
  :in-order-to ((asdf:test-op (asdf:load-op :galois-fields-test)))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :galois-fields-test :run-tests))
  :components ((:static-file "README.md")
               (:static-file "UNLICENSE.txt")
               (:module "src"
                :components ((:file "package")
                             (:file "def" :depends-on ("package"))))))
