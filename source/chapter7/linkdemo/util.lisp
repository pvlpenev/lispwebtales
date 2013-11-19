;;;; util.lisp

(in-package #:linkdemo)

(defun logged-on-p ()
  (hunchentoot:session-value :username))

(defun log-in (username &optional (redirect-route 'home))
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value :username) username)
  (redirect redirect-route))

(defun log-out (&optional (redirect-route 'home))
  (setf (hunchentoot:session-value :username) nil)
  (redirect redirect-route))

(defun start-linkdemo (&key
                        (port 8080)
                        (datastore 'linkdemo.pg-datastore:pg-datastore)
                        (datastore-init nil))
  (setf *datastore* (apply #'make-instance datastore datastore-init))
  (init)
  (start '#:linkdemo :port port :render-method 'html-frame))

