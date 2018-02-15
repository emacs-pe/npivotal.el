;;; pivotal.el --- Pivotal Tracker Integration        -*- lexical-binding: t -*-

;; Copyright (c) 2018 PuercoPop

;; Author: PuercoPop
;; URL: https://github.com/emacs-pe/pivotal.el
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
  (require 'url-vars)
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))
(require 'json)
(require 'tabulated-list)

(defgroup pivotal nil
  "Emacs Mode for interacting with Pivotal Tracker."
  :group 'external) ;; Check if there is a better category

(defcustom pivotal-api-token nil
  "The token used for authentication.

You can obtain your API key in `https://www.pivotaltracker.com/profile'."
  :group 'pivotal
  :type 'string)

(defvar pivotal-read-response-function #'pivotal-read-json-response)

(defconst pivotal-api-base-url "https://www.pivotaltracker.com/services/v5")

(define-error 'pivotal-error "Pivotal Error")
(define-error 'pivotal-http-error "HTTP Error" 'pivotal-error)


;; API
(defsubst pivotal-as-string (value)
  "If VALUE is already a string, return it.  Otherwise convert it to a string and return that."
  (cl-etypecase value
    (stringp value)
    (numberp (number-to-string value))
    (symbolp (symbol-name value))))

(defsubst pivotal-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol (intern string-or-symbol)))

(defun pivotal-read-json-response (start end)
  "Read json from START to END points."
  (json-read-from-string (decode-coding-string (buffer-substring-no-properties start end) 'utf-8)))

(defun pivotal-request (method resource &optional params data noerror)
  "Make a request using METHOD for RESOURCE.

METHOD is a HTTP request method, a string.  If non-nil, send
PARAMS and/or DATA in the request.  Raises an error unless
optional NOERROR is non-nil, in which case return nil."
  (cl-assert pivotal-api-token nil "Pivotal API key is not defined.")
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


;; Resources


;; UI

(defmacro pivotal-tbl-define (symbol docstring &rest properties)
  "Define tabulated list UI.

Define SYMBOL as a generic `tabulated-list' UI with DOCSTRING.
The following PROPERTIES constitute:

`:buffer BUFFER-NAME'
    BUFFER-NAME can be a string or a function to obtain the buffer name.

`:actions ACTIONS'
    ACTIONS available for entries, it must be of the form (key doc command).

`:columns COLUMN'
    COLUMNS used for displaying the column format.  See `tabulated-list-format'.

`:entries ENTRIES'
    Entries can be either a list of function.  See `tabulated-list-entries'.

`:actions ACTIONS'
    List of ACTIONS which can be executed over a list of entries."
  (declare (indent 1) (doc-string 2))
  (let* ((mode      (pivotal-as-symbol (format "%s-mode" symbol)))
         (mode-map  (pivotal-as-symbol (format "%s-map" mode)))
         (mode-name (format "%s menu" symbol))
         (buffer    (plist-get properties :buffer))
         (actions   (plist-get properties :actions))
         (columns   (plist-get properties :columns))
         (entries   (plist-get properties :entries))
         (display-list (pivotal-as-symbol (format "%s-list" symbol))))
    (when (null buffer)
      (error ":buffer property is required"))
    (when (null entries)
      (error ":entries property is required"))
    (when (null columns)
      (error ":columns property is required"))

    `(progn
       (define-derived-mode ,mode tabulated-list-mode ,mode-name
         ,docstring
         (setq tabulated-list-format ,columns
               tabulated-list-entries ,entries
               tabulated-list-padding 2)
         (tabulated-list-init-header))

       (dolist (action ,actions)
         (define-key ,mode-map (nth 0 action) (nth 2 action)))

       (defun ,display-list ()
         (interactive)
         (with-current-buffer (get-buffer-create (if (functionp ,buffer) (funcall ,buffer) ,buffer))
           (funcall ',mode)
           (tabulated-list-print)
           (pop-to-buffer (current-buffer)))))))

(defun pivotal-entries-projects ()
  "Entries list for pivotal projects."
  (seq-map (lambda (project)
             (let-alist project
               (list (pivotal-as-string .id)
                     (vector (pivotal-as-string .id)
                             (pivotal-as-string .version)
                             (pivotal-as-string .current_iteration_number)
                             .name))))
           (pivotal-request "GET" "/projects" '(("fields" "id,name,version,current_iteration_number")))))


;; Entry points

(pivotal-tbl-define pivotal-projects
  "List of pivotal projects."
  :buffer  "*pivotal*"
  :columns [("id"        10 t)
            ("version"   10 t)
            ("iteration" 11 t)
            ("name"      10 t)]
  :entries 'pivotal-entries-projects
  :actions '(([return] "Show project information."  pivotal-show-project)))

(provide 'pivotal)
;;; pivotal.el ends here
