;;;; defmodule.lisp

(restas:define-module #:closure-hello
  (:use #:cl)
  (:render-method #'closure-hello.view:main))

(in-package #:closure-hello)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" closure-hello-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" closure-hello-config:*base-directory*))


