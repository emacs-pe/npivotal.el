;;; npivotal.el --- -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'url)
(require 'json)

(defgroup pivotal nil
  "Emacs Mode for interacting with Pivotal Tracker"
  :group 'external) ;; Check if there is a better category

(defcustom pivotal-api-token nil
  "The token used for authentication."
  :group 'pivotal
  :type 'string)

(defvar pivotal-base-url "https://www.pivotaltracker.com/services/v5/")


;; Endpoints

(defun pivotal-endpoint-url (path)
  (format "%s%s" pivotal-base-url path))

(defun pivotal-api (url method cb)
  (let ((url-request-method "GET")
        (url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token))))
    (url-retrieve (pivotal-endpoint-url "/me")
                  (lambda () (message "foo")))))

(let* ((url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token)))
       (request-buffer (url-retrieve (pivotal-endpoint-url "/me")
                                     (lambda (status)
                                       (when-let ((error-msg (plist-get status :error)))
                                         (error "Error: %s" error-msg))
                                       (search-forward-regexp "^\r?\n\r?" nil 'noerror)
                                       (let ((json (json-read)))
                                         (with-temp-buffer
                                           (url-insert-buffer-contents json))))))
       (values request-buffer)))


;; Resources


;; UI

(defun pivotal-list-projects (projects)
  (with-current-buffer (get-buffer-create "*pivotal-projects*")
    (dotimes (ix (length projects))
      (let ((project (elt projects ix)))
        (let-alist project
          (insert (format "%s\n" .project_name)))))))

;; Entry points

