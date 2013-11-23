;;;; pg-datastore.lisp

(in-package #:linkdemo.pg-datastore)

(defclass pg-datastore ()
  ((connection-spec :initarg :connection-spec
                    :accessor connection-spec)))

(defclass users ()
  ((id :col-type serial :reader user-id)
   (name :col-type string :reader user-name :initarg :name)
   (password :col-type string :reader user-password :initarg :password)
   (salt :col-type string :reader user-salt :initarg :salt))
  (:metaclass dao-class)
  (:keys id))

(defclass links ()
  ((id :col-type serial :reader link-id)
   (url :col-type string :reader link-url :initarg :url)
   (title :col-type string :reader link-title :initarg :title)
   (submitter-id :col-type integer :reader link-submitter-id :initarg :submitter-id))
  (:metaclass dao-class)
  (:keys id))

(deftable links
  (!dao-def)
  (!foreign 'users 'submitter-id 'id))

(defclass votes ()
  ((link-id :col-type integer :reader vote-link-id :initarg :link-id)
   (submitter-id :col-type integer :reader vote-submitter-id :initarg :submitter-id))
  (:metaclass dao-class)
  (:keys link-id submitter-id))

(deftable votes
  (!dao-def)
  (!foreign 'links 'link-id 'id)
  (!foreign 'users 'submitter-id 'id))

(defmethod datastore-init ((datastore pg-datastore))
  (with-connection (connection-spec datastore)
    (unless (table-exists-p 'users)
      (execute (dao-table-definition 'users)))
    (unless (table-exists-p 'links)
      (create-table 'links))
    (unless (table-exists-p 'votes)
      (create-table 'votes))))

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

(defmethod datastore-find-user ((datastore pg-datastore) username)
  (with-connection (connection-spec datastore)
    (query (:select :* :from 'users
                    :where (:= 'name username))
           :plist)))

(defmethod datastore-auth-user ((datastore pg-datastore) username password)
  (let ((user (datastore-find-user datastore username)))
    (when (and user
               (check-password password (getf user :password) 
                                        (getf user :salt)))
      username)))

(defmethod datastore-register-user ((datastore pg-datastore) username password)
  (with-connection (connection-spec datastore)
    (unless (datastore-find-user datastore username)
      (let ((password-salt (hash-password password)))
        (when
            (save-dao
             (make-instance 'users
                            :name username
                            :password (getf password-salt :password-hash)
                            :salt (getf password-salt :salt)))
          username)))))

(defmethod datastore-upvoted-p ((datastore pg-datastore) link-id user)
  (with-connection (connection-spec datastore)
    (query (:select :link-id :users.name :from 'votes 'users
                    :where (:and (:= 'users.id 'submitter-id)
                                 (:= 'users.name user)
                                 (:= 'link-id link-id)))
           :plist)))

(defmethod datastore-upvote ((datastore pg-datastore) link-id user)
  (with-connection (connection-spec datastore)
    (let ((submitter-id (getf (datastore-find-user datastore user) :id)))
      (when (and submitter-id
                 (not (datastore-upvoted-p datastore link-id user)))
        (when (save-dao (make-instance 'votes
                                       :link-id link-id
                                       :submitter-id submitter-id))
          link-id)))))

(defmethod datastore-post-link ((datastore pg-datastore) url title user)
  (with-connection (connection-spec datastore)
    (let* ((submitter-id (getf (datastore-find-user datastore user) :id))
           (link (make-instance 'links
                                :url url
                                :title title
                                :submitter-id submitter-id)))
      (save-dao link)
      (datastore-upvote datastore (link-id link) user))))

(defun get-all-links/internal ()
  (query (:select :* :from 'links) :plists))

(defmethod datastore-upvote-count ((datastore pg-datastore) link-id)
  (with-connection (connection-spec datastore)
    (query (:select (:count link-id) :from 'votes
                    :where (:= link-id 'link-id))
           :single)))

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

(defmethod datastore-get-all-links ((datastore pg-datastore) 
                                    &optional username)
  (with-connection (connection-spec datastore)
    (sort-links
     (add-vote-count datastore
                     (get-all-links/internal)
                     (or username "")))))

