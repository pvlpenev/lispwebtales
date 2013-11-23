;;;; authdemo.lisp

(in-package #:authdemo)

;;; "authdemo" goes here. Hacks and glory await!

(defvar *authenticate-user-function* nil)
(defvar *register-user-function* nil)
(defvar *redirect-route* nil)

(defun logged-on-p ()
  (hunchentoot:session-value :username))

(defun log-in (username &optional (redirect-route *redirect-route*))
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value :username) username)
  (redirect redirect-route))

(defun log-out (&optional (redirect-route *redirect-route*))
  (setf (hunchentoot:session-value :username) nil)
  (redirect redirect-route))

(define-route login ("login")
  (list :title "Log in"
        :body (login-form)))

(define-route login/post ("login" :method :post)
  (let ((user (funcall *authenticate-user-function*
                       (hunchentoot:post-parameter "username")
                       (hunchentoot:post-parameter "password"))))
    (if user
        (log-in user)
        (redirect 'login))))

(define-route register ("register")
  (list :title "register"
        :body (register-form)))

(define-route register/post ("register" :method :post)
  (let ((user (funcall *register-user-function*
                       (hunchentoot:post-parameter "username")
                       (hunchentoot:post-parameter "password"))))
    (if user
        (log-in user)
        (redirect 'register))))

(define-route logout ("logout")
  (log-out))
