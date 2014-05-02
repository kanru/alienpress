(in-package :cl-user)

(asdf:defsystem :alienpress
  :description "Blog compiler that uses alien technologies"
  :version "0.0.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:file "packages" :pathname "src/packages")
               (:module "src"
                :depends-on ("packages")
                :components ((:file "alienpress"   :depends-on ("site"))
                             (:file "article"      :depends-on ("file"))
                             (:file "directive")
                             (:file "file")
                             (:file "preprocessor" :depends-on ("article"))
                             (:file "site"         :depends-on ("utils"))
                             (:file "utils"))))
  :depends-on ("alexandria"
               "uiop"
               "cl-markdown"
               "cl-mustache"
               "split-sequence"))
