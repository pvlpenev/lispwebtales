;;;; defmodule.lisp

(restas:define-module #:hello-world
  (:use #:cl))

(in-package #:hello-world)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" hello-world-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" hello-world-config:*base-directory*))


