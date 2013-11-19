;;;; redis-datastore.lisp

(in-package #:linkdemo.redis-datastore)

(defclass redis-datastore ()
  ((host :initarg :host :initform #(127 0 0 1) :accessor host)
   (port :initarg :port :initform 6379 :accessor port)))

(defmethod datastore-init ((datastore redis-datastore)))

(defun hash-password (password)
  (multiple-value-bind (hash salt)
      (ironclad:pbkdf2-hash-password (babel:string-to-octets password))
    (list :password-hash (ironclad:byte-array-to-hex-string hash)
          :salt (ironclad:byte-array-to-hex-string salt))))

(defun check-password (password password-hash salt)
  (let ((hash (ironclad:pbkdf2-hash-password
               (babel:string-to-octets password)
               :salt (ironclad:hex-string-to-byte-array salt))))
    (string= (ironclad:byte-array-to-hex-string hash)
             password-hash)))

(defun serialize-list (list)
  (with-output-to-string (out)
    (print list out)))

(defun deserialize-list (string)
  (let ((read-eval nil))
    (read-from-string string)))

(defun make-key (prefix suffix)
  (format nil "~a:~a" (symbol-name prefix) suffix))

(defmethod datastore-find-user ((datastore redis-datastore) username)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (let ((user-id (red-get (make-key :username username))))
      (when user-id
        (deserialize-list (red-get (make-key :user user-id)))))))

(defmethod datastore-auth-user ((datastore redis-datastore) username password)
  (let ((user (datastore-find-user datastore username)))
    (when (and user
               (check-password password 
                               (getf user :password)
                               (getf user :salt)))
      username)))

(defmethod datastore-register-user ((datastore redis-datastore) username password)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (unless (datastore-find-user datastore username)
      (let* ((password-salt (hash-password password))
             (id (red-incr :user-ids))
             (record (list :id id
                           :username username
                           :password (getf password-salt :password-hash)
                           :salt (getf password-salt :salt))))
        (red-set (make-key :user id) (serialize-list record))
        (red-set (make-key :username username) id)
        username))))

(defmethod datastore-upvoted-p ((datastore redis-datastore) link-id user)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (red-sismember (make-key :upvote link-id) user)))

(defmethod datastore-upvote ((datastore redis-datastore) link-id user)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (when (and (datastore-find-user datastore user)
               (not (datastore-upvoted-p datastore link-id user)))
      (when (red-sadd (make-key :upvote link-id) user)
        link-id))))

(defmethod datastore-post-link ((datastore redis-datastore) url title user)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (let* ((submitter-id (getf (datastore-find-user datastore user) :id))
           (id (red-incr :posts-ids))
           (link (list :id id
                       :url url
                       :title title
                       :submitter-id submitter-id)))
      (red-set (make-key :post id) (serialize-list link))
      (datastore-upvote datastore (getf link :id) user))))

(defun get-all-links/internal ()
  (let ((keys (red-keys (make-key :post "*"))))
    (loop for key in keys
         collect (deserialize-list (red-get key)))))

(defmethod datastore-upvote-count ((datastore redis-datastore) link-id)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (red-scard (make-key :upvote link-id))))

(defun add-vote-count (datastore links username)
  (loop
     for link in links
     for id = (getf link :id)
     collect (list* :votes (datastore-upvote-count datastore id)
                    :voted-p (datastore-upvoted-p datastore id username)
                    link)))

(defun sort-links (links)
  (sort links #'>
        :key #'(lambda (link) (getf link :votes))))

(defmethod datastore-get-all-links ((datastore redis-datastore) &optional username)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (sort-links
     (add-vote-count datastore
                     (get-all-links/internal)
                     (or username "")))))
