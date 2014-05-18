(in-package :cl-user)

(asdf:defsystem :alienpress
  :description "Blog compiler that uses alien technologies"
  :version "0.0.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:file "packages" :pathname "src/packages")
               (:module "src"
                :depends-on ("packages")
                :components ((:file "alienpress"
                              :depends-on ("article" "file" "site"))
                             (:file "article"
                              :depends-on ("file" "site" "utils"))
                             (:file "file"
                              :depends-on ("utils"))
                             (:file "site"
                              :depends-on ("utils"))
                             (:file "utils"))))
  :depends-on ("alexandria"
               "uiop"
               "cl-markdown"
               "cl-mustache"
               "split-sequence"
               "cl-date-time-parser"))
