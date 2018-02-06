;;; npivotal.el --- -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'url)
(require 'json)
(require 'tabulated-list)

(defgroup pivotal nil
  "Emacs Mode for interacting with Pivotal Tracker"
  :group 'external) ;; Check if there is a better category

(defcustom pivotal-api-token nil
  "The token used for authentication."
  :group 'pivotal
  :type 'string)

(defvar pivotal-base-url "https://www.pivotaltracker.com/services/v5")


;; Endpoints

(defun pivotal-endpoint-url (path)
  (format "%s%s" pivotal-base-url path))

(cl-defmacro with-pivotal-authentication ((endpoint &key (method "GET")) &body body)
  (let ((temp-buffer (gensym)))
    `(let* ((url-request-method ,method)
            (url-request-extra-headers '(("X-TrackerToken" . ,pivotal-api-token)))
            (,temp-buffer (url-retrieve-synchronously ,(pivotal-endpoint-url endpoint))))
       (let-alist (with-current-buffer ,temp-buffer
                    (search-forward-regexp "^\r?\n\r?" nil 'noerror)
                    (json-read))
         ,@body))))





;; (let* ((url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token)))
;;        (request-buffer (url-retrieve (pivotal-endpoint-url "/me")
;;                                      (lambda (status)
;;                                        (when-let ((error-msg (plist-get status :error)))
;;                                          (error "Error: %s" error-msg))
;;                                        (search-forward-regexp "^\r?\n\r?" nil 'noerror)
;;                                        (let ((json (json-read)))
;;                                          (with-temp-buffer
;;                                            (url-insert-buffer-contents json))))))
;;        (values request-buffer)))


;; Resources


;; UI

(define-derived-mode pivotal-projects tabulated-list-mode "*pivotal-projects*" "List your Pivotal Projects."
  (setq tabulated-list-entries 'pivotal--list-projects
        tabulated-list-format [("ID" 10 #'<)
                               ("FAV" 3 nil)
                               ("Project Name" -1 t)])
  (tabutaled-list-init-header))

(defun pivotal--list-projects ()
  (let* ((url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token)))
         (reponse-buffer (url-retrieve-synchronously (pivotal-endpoint-url "/me"))))
    (let-alist (with-current-buffer reponse-buffer
                 (search-forward-regexp "^\r?\n\r?" nil 'noerror)
                 (json-read))
      .projects)))

(defun pivotal-list-projects (projects)
  (with-current-buffer (get-buffer-create "*pivotal-projects*")
    (dotimes (ix (length projects))
      (let ((project (elt projects ix)))
        (let-alist project
          (insert (format "%s\n" .project_name)))))))

;; Entry points

