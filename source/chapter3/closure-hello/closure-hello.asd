(defpackage #:closure-hello-config (:export #:*base-directory*))
(defparameter closure-hello-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:closure-hello
  :serial t
  :description "Your description here"
  :author "Your name here"
  :license "Your license here"
  :defsystem-depends-on (#:closure-template) 
  :depends-on (:RESTAS :CLOSURE-TEMPLATE)
  :components ((:closure-template "templates/main")
               (:file "defmodule")
               (:file "closure-hello")))
