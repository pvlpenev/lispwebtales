;;;; linkdemo.lisp

(in-package #:linkdemo)

;;; "linkdemo" goes here. Hacks and glory await!

(define-route home ("")
  (list :title "Linkdemo"
        :body (home-page (get-all-links (logged-on-p)))))

(define-route login ("login")
  (list :title "Log in"
        :body (login-form)))

(define-route login/post ("login" :method :post)
  (let ((user (auth-user (hunchentoot:post-parameter "username")
                         (hunchentoot:post-parameter "password"))))
    (if user
        (log-in user)
        (redirect 'login))))

(define-route register ("register")
  (list :title "register"
        :body (register-form)))

(define-route register/post ("register" :method :post)
  (let ((user (register-user (hunchentoot:post-parameter "username")
                             (hunchentoot:post-parameter "password"))))
    (if user
        (log-in user)
        (redirect 'register))))

(define-route logout ("logout")
  (log-out))

(define-route submit ("submit")
  (list :title "Submit a link"
        :body (submit-form)))

(define-route submit/post ("submit" :method :post)
  (let ((link (post-link (hunchentoot:post-parameter "url")
                         (hunchentoot:post-parameter "title")
                         (logged-on-p))))
    (if link
        (redirect 'home)
        (redirect 'submit))))

(define-route upvote-link ("upvote/:id")
  (:sift-variables (id #'parse-integer))
  (when (logged-on-p)
    (upvote id (logged-on-p)))
  (redirect 'home))

