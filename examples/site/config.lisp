(in-package :cl-user)

(make-instance 'alienpress::site
               :name "Example"
               :title "Example Site"
               :sitedir "./"
               :destdir "../www/"
               :baseurl "//")
