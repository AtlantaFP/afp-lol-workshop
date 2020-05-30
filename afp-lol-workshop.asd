#|
  This file is a part of afp-lol-workshop project.
  Copyright (c) 2019 Atlanta Functional Programming
|#

#|
  Author: Atlanta Functional Programming
|#

(defsystem "afp-lol-workshop"
  :version "0.1.0"
  :author "Atlanta Functional Programming"
  :license ""
  :depends-on (:alexandria :serapeum :cl-ppcre :named-readtables)
  :components ((:module "src"
                :components
                        ((:file "macros-intro")
                         (:file "macros-intro-once-only")
                         (:file "reader-macros-examples")
                         (:file "programs-that-program")
                         (:file "anaphoric-macros")
			 (:file "pandoric-macros")
                         (:file "afp-lol-workshop"))))
  :description "An overview of code from Let Over Lambda (plus a few extras)"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "afp-lol-workshop-test"))))
