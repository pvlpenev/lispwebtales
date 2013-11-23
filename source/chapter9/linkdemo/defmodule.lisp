;;;; defmodule.lisp

(restas:define-policy datastore
  (:interface-package #:linkdemo.policy.datastore)
  (:interface-method-template "DATASTORE-~A")
  (:internal-package #:linkdemo.datastore)

  (define-method init ()
    "initiate the datastore")
  
  (define-method find-user (username)
    "find the user by username")
  
  (define-method auth-user (username password)
    "Check if a user exists and has the suplied password")
  
  (define-method register-user (username password)
    "Register a new user")
  
  (define-method upvoted-p (link-id username)
    "Check if a user has upvoted a link")
  
  (define-method upvote (link-id user)
    "upvote a link")
  
  (define-method post-link (url title user)
    "post a new link")
  
  (define-method get-all-links (&optional user)
    "Get all of the links in the datastore")
  
  (define-method upvote-count (link-id)
    "get the number of upvotes for a given link"))

(restas:define-module #:linkdemo
  (:use #:cl #:restas #:linkdemo.datastore #:authdemo)
  (:export #:start-linkdemo))

(defpackage #:linkdemo.pg-datastore
  (:use #:cl #:postmodern #:linkdemo.policy.datastore)
  (:export #:pg-datastore))

(defpackage #:linkdemo.redis-datastore
  (:use #:cl #:redis #:linkdemo.policy.datastore)
  (:export #:redis-datastore))

(in-package #:linkdemo)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" linkdemo-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" linkdemo-config:*base-directory*))

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))

(mount-module -static- (#:restas.directory-publisher)
  (:url "static")
  (restas.directory-publisher:*directory* *static-directory*))

(mount-module -authdemo- (#:authdemo)
  (:render-method 'html-frame)
  (authdemo:*authenticate-user-function* #'auth-user)
  (authdemo:*register-user-function* #'register-user)
  (authdemo:*redirect-route* 'home))
