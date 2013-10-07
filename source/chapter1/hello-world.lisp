;;;; hello-world.lisp

(ql:quickload "restas")

(restas:define-module #:hello-world
    (:use :cl :restas))

(in-package #:hello-world)

(define-route hello-world ("")
  "Hello World")

(start '#:hello-world :port 8080)
