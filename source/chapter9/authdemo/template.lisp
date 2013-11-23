;;;; template.lisp

(in-package #:authdemo)

(defun login-form ()
  (<:form :action (genurl 'login/post) :method "post"
          "User name:" (<:br)
          (<:input :type "text" :name "username")(<:br)
          "Password:" (<:br)
          (<:input :type "password" :name "password") (<:br)
          (<:input :type "submit" :value "Log in")))

(defun register-form ()
  (<:form :action (genurl 'register/post) :method "post"
          "User name:" (<:br)
          (<:input :type "text" :name "username")(<:br)
          "Password:" (<:br)
          (<:input :type "password" :name "password") (<:br)
          (<:input :type "submit" :value "Register")))
