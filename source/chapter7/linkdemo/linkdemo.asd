(defpackage #:linkdemo-config (:export #:*base-directory*))
(defparameter linkdemo-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:linkdemo
  :serial t
  :description "Your description here"
  :author "Your name here"
  :license "Your license here"
  :depends-on (:RESTAS :SEXML :POSTMODERN :IRONCLAD :BABEL :cl-redis)
  :components ((:file "defmodule")
               (:file "pg-datastore")
               (:file "redis-datastore")
               (:file "util")
               (:file "template")
               (:file "linkdemo")))
