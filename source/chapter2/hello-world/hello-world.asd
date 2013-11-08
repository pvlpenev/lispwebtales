(defpackage #:hello-world-config (:export #:*base-directory*))
(defparameter hello-world-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:hello-world
  :serial t
  :description "Your description here"
  :author "Your name here"
  :license "Your license here"
  :depends-on (:RESTAS)
  :components ((:file "defmodule")
               (:file "hello-world")))