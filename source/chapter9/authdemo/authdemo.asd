(defpackage #:authdemo-config (:export #:*base-directory*))
(defparameter authdemo-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:authdemo
  :serial t
  :description "Your description here"
  :author "Your name here"
  :license "Your license here"
  :depends-on (:RESTAS :SEXML)
  :components ((:file "defmodule")
               (:file "template")
               (:file "authdemo")))
