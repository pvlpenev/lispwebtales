;;;; defmodule.lisp

(restas:define-module #:blogdemo
  (:use #:cl #:restas)
  (:export #:start-blogdemo))

(in-package #:blogdemo)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" blogdemo-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" blogdemo-config:*base-directory*))

(defparameter *posts* nil)

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))
