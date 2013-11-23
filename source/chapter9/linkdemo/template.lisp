;;;; template.lisp

(in-package #:linkdemo)

(<:augment-with-doctype "html" "" :auto-emit-p t)

(defun html-frame (context)
  (<:html
   (<:head (<:title (getf context :title))
           ;; Stylesheet
           (<:link :rel "stylesheet" :type "text/css" :href "/static/css/style.css"))
   (<:body
    (<:div
     (<:h1 (getf context :title))
     (<:a :href (genurl 'home) "Home") " | "
     (if (logged-on-p)
         (list (<:a :href (genurl 'submit) "Submit a link")
               " | "
               (<:a :href (genurl '-authdemo-.logout)
                    (format nil "Logout ~A"
                            (logged-on-p))))
         (list (<:a :href (genurl '-authdemo-.login) "Log in")
               " or "
               (<:a :href (genurl '-authdemo-.register) "Register")))
     (<:hr))
    (getf context :body))))

(defun home-page (links)
  (loop
     for link in links
     collect
       (<:div (if (logged-on-p)
                  (if (getf link :voted-p)
                      "*"
                      (<:a :href (genurl 'upvote-link :id (getf link :id))
                           "upvote"))
                  "*")
              " "
              (getf link :votes)
              " "
              (<:a :href (getf link :url) (getf link :title)))))

(defun submit-form ()
  (<:form :action (genurl 'submit/post) :method "post"
          "Title:" (<:br)
          (<:input :type "text" :name "title") (<:br)
          "URL:" (<:br)
          (<:input :type "text" :name "url") (<:br)
          (<:input :type "submit" :value "Submit")))
