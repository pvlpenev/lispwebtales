(defpackage #:blogdemo-config (:export #:*base-directory*))
(defparameter blogdemo-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:blogdemo
  :serial t
  :description "Your description here"
  :author "Your name here"
  :license "Your license here"
  :depends-on (:RESTAS :SEXML)
  :components ((:file "defmodule")
               (:file "util")
               (:file "template")
               (:file "blogdemo")))
