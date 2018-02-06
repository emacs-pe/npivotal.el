;;; npivotal.el --- Pivotal Tracker Integration       -*- lexical-binding: t -*-

;; Copyright (c) 2018 PuercoPop

;; Author: PuercoPop
;; URL: https://github.com/emacs-pe/npivotal.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.4"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Pivotal integration.  See <URL:https://www.pivotaltracker.com/>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))
(require 'json)
(require 'tabulated-list)

(defgroup pivotal nil
  "Emacs Mode for interacting with Pivotal Tracker"
  :group 'external) ;; Check if there is a better category

(defcustom pivotal-api-token nil
  "The token used for authentication."
  :group 'pivotal
  :type 'string)

(defvar pivotal-read-response-function #'pivotal-read-json-response)

(defconst pivotal-api-base-url "https://www.pivotaltracker.com/services/v5")

(define-error 'pivotal-error "Pivotal Error")
(define-error 'pivotal-http-error "HTTP Error" 'pivotal-error)

;; API
(defun pivotal-read-json-response (start end)
  "Read json from START to END points."
  (json-read-from-string (decode-coding-string (buffer-substring-no-properties start end) 'utf-8)))

(defun pivotal-request (method resource &optional params data noerror)
  "Make a request using METHOD for RESOURCE.

METHOD is a HTTP request method, a string.  If non-nil, send
PARAMS and/or DATA in the request.  Raises an error unless
optional NOERROR is non-nil, in which case return nil."
  (let* ((p (and params (concat "?" (url-build-query-string params))))
         (d (and data (encode-coding-string (json-encode-list data) 'utf-8)))
         (url-request-extra-headers `(("Content-Type"   . "application/json")
                                      ("X-TrackerToken" . ,pivotal-api-token)))
         (url-request-method method)
         (url-request-data d))
    (with-current-buffer (url-retrieve-synchronously (concat pivotal-api-base-url resource p))
      (set-buffer-multibyte t)
      (goto-char (1+ url-http-end-of-headers))
      (let ((body (funcall pivotal-read-response-function (point) (point-max))))
        (unless (or noerror (= (/ url-http-response-status 100) 2))
          (signal 'pivotal-http-error (cons url-http-response-status (list method resource p d body))))
        body))))

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

(provide 'npivotal)
;;; npivotal.el ends here
