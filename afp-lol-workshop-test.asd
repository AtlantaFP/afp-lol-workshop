#|
  This file is a part of afp-lol-workshop project.
  Copyright (c) 2019 Atlanta Functional Programming
|#

(defsystem "afp-lol-workshop-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Atlanta Functional Programming"
  :license ""
  :depends-on ("afp-lol-workshop"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "afp-lol-workshop"))))
  :description "Test system for afp-lol-workshop"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
