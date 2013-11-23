;;;; linkdemo.lisp

(in-package #:linkdemo)

;;; "linkdemo" goes here. Hacks and glory await!

(define-route home ("")
  (list :title "Linkdemo"
        :body (home-page (get-all-links (logged-on-p)))))

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

