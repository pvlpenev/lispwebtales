;;;; util.lisp

(in-package #:linkdemo)

(defun start-linkdemo (&key
                         (port 8080)
                         (datastore 'linkdemo.pg-datastore:pg-datastore)
                         (datastore-init nil))
  (setf *datastore* (apply #'make-instance datastore datastore-init))
  (init)
  (start '#:linkdemo :port port :render-method 'html-frame))

