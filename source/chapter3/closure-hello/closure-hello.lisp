;;;; closure-hello.lisp

(in-package #:closure-hello)

;;; "closure-hello" goes here. Hacks and glory await!

(defparameter *todos* (list "Get milk" "Pick up paycheck" "Cash paycheck"))

(restas:define-route main ("")
  (list :title "Hello World"
        :main t
        :body "<h1>Hello World</h1>"))

(restas:define-route todos ("todos")
  (list :title "Tasks for today"
        :todos t
        :body (closure-hello.view:todos (list :todos *todos*))))

(restas:define-route lost ("lost")
  (list :title "Are we lost?"))
