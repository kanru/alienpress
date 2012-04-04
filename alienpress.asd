(asdf:defsystem :alienpress
  :description "Blog compiler that uses alien technologies"
  :version "0.0.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "config" :depends-on ("packages"))
                             (:file "utils" :depends-on ("config"))
                             (:file "alienpress" :depends-on ("utils")))))
  :depends-on ("cl-mustache"
               "cl-markdown"
               "cl-fad"
               "alexandria"
               "marshal"))
