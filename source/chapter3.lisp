(ql:quickload "closure-template")

(defpackage #:closure-test
  (:use #:cl))

(in-package #:closure-test)

(defparameter *template*
  "{namespace hello}
{template main}
  <h1>Hello World</h1>
{/template}")


(closure-template:compile-template :common-lisp-backend *template*)

(pring (hello:main))
