;;;; defmodule.lisp

(restas:define-module #:authdemo
  (:use #:cl #:restas)
  (:export #:logged-on-p
           #:*authenticate-user-function*
           #:*register-user-function*
           #:*redirect-route*))

(in-package #:authdemo)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" authdemo-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" authdemo-config:*base-directory*))

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))
