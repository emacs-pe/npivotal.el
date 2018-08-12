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
  (require 'wid-edit)
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))
(require 'json)
(require 'widget)
(require 'parse-time)
(require 'tabulated-list)

(defgroup pivotal nil
  "Emacs Mode for interacting with Pivotal Tracker."
  :group 'external) ;; Check if there is a better category

(defcustom pivotal-api-token nil
  "The token used for authentication.

You can obtain your API key in `https://www.pivotaltracker.com/profile'."
  :group 'pivotal
  :type 'string)

(defvar-local pivotal-project-id nil)
(defvar-local pivotal-project-labels nil)

(defvar pivotal-read-response-function #'pivotal-read-json-response)

(defvar pivotal-default-story-types '("feature" "bug" "chore" "release")
  "List of default Pivotal story types.")

(defvar pivotal-default-story-states
  '("accepted" "delivered" "finished" "started" "rejected" "planned" "unstarted" "unscheduled")
  "List of default Pivotal story states.")

(defvar pivotal-filter-story nil)

(defconst pivotal-api-base-url "https://www.pivotaltracker.com/services/v5")

(define-error 'pivotal-error "Pivotal Error")
(define-error 'pivotal-http-error "HTTP Error" 'pivotal-error)
(define-error 'pivotal-api-error "API Error" 'pivotal-error)

(define-button-type 'pivotal-browse-url
  'face 'link
  'help-echo "mouse-2, RET: browse url"
  'follow-link t
  'action (lambda (button) (browse-url (button-get button 'target)))
  'skip t)

(cl-defstruct (pivotal-story
               (:copier nil)
               (:constructor pivotal-story-new)
               (:type vector))
  "A structure holding the information of a pivotal story."
  id name description state project-id)

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

(defsubst pivotal-filter-toggle (value)
  "Toggle filter VALUE."
  (if (string-prefix-p "-" value) (substring value 1) (concat "-" value)))

(defsubst pivotal-link-button (url &optional name)
  "Create a button for URL with NAME."
  (make-text-button (or name url) nil 'type 'pivotal-browse-url 'target url))

(defun pivotal-read-json-response (start end)
  "Read json from START to END points."
  (json-read-from-string (decode-coding-string (buffer-substring-no-properties start end) 'utf-8)))

(defun pivotal-2ft (value)
  "Convert VALUE to a floating point time.

If VALUE is already a number, just return it.  If it is a string,
parse it as a time string and apply `float-time' to it.  If VALUE
is nil, just return 0."
  (cl-typecase value
    (numberp value)
    (stringp (condition-case nil
                 (float-time (parse-iso8601-time-string value))
               (error 0.)))
    (t       0.)))

(defun pivotal-time< (a b)
  "Compare date A and B. Suitable as predicate for `sort'."
  (setq a (pivotal-2ft a) b (pivotal-2ft b)) (and (> a 0) (> b 0) (< a b)))

(defun pivotal-tabulated-time< (n entry-a entry-b)
  "Compare time value by Nth entry N of ENTRY-A and ENTRY-B tabulated entries."
  (pivotal-time< (elt (cadr entry-a) n) (elt (cadr entry-b) n)))

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

(defun pivotal-read-project-id (prompt)
  "Read a pivotal project id with a PROMPT."
  (let ((default (and (derived-mode-p 'pivotal-projects-mode) (tabulated-list-get-id))))
    (or (and (not current-prefix-arg) default)
        (let ((entries (seq-map (lambda (project)
                                  (let-alist project (cons .name (pivotal-as-string .id))))
                                (pivotal-request "GET" "/projects" '(("fields" "id,name"))))))
          (assoc-default (completing-read prompt entries nil t) entries)))))

(defun pivotal-read-story-id (prompt project-id)
  "Read a pivotal story id from a PROJECT-ID with PROMPT."
  (let ((default (and (derived-mode-p 'pivotal-stories-mode) (tabulated-list-get-id))))
    (or (and (not current-prefix-arg) default)
        (let ((entries (seq-map (lambda (project)
                                  (let-alist project (cons .name (pivotal-as-string .id))))
                                (pivotal-request "GET" (format "/projects/%s/stories" project-id) '(("fields" "id,name"))))))
          (assoc-default (completing-read prompt entries nil t) entries)))))

(defmacro pivotal-define-tbl (symbol docstring &rest properties)
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

`:keymap MAP'
    List of keymaps which can be executed over a list of entries.

`:menu NAME'
    Menu name used as entry point function.  by default uses adds `-list-' to the SYMBOL name."
  (declare (indent defun) (doc-string 2))
  (let* ((mode      (pivotal-as-symbol (format "pivotal-%s-mode" symbol)))
         (mode-map  (pivotal-as-symbol (format "%s-map" mode)))
         (mode-name (format "%s menu" symbol))
         (keymap    (plist-get properties :keymap))
         (columns   (plist-get properties :columns))
         (entries   (plist-get properties :entries))
         (menu-name (or (plist-get properties :menu)
                        (pivotal-as-symbol (format "pivotal-list-%s" symbol))))
         (menu-buffer (or (plist-get properties :buffer)
                          (format "*%s*" menu-name))))
    (when (null entries)
      (error ":entries property is required"))
    (when (null columns)
      (error ":columns property is required"))

    `(progn
       (define-derived-mode ,mode tabulated-list-mode ,mode-name
         (setq tabulated-list-format ,columns
               tabulated-list-entries ,entries
               tabulated-list-padding 2)
         (tabulated-list-init-header))

       (pcase-dolist (`(,key . ,def) ,keymap)
         (define-key ,mode-map
           (if (vectorp key) key (read-kbd-macro key))
           (if (and (symbolp def) (boundp def) (keymapp (symbol-value def))) (symbol-value def) def)))

       (defun ,menu-name ()
         ,docstring
         (interactive)
         (let ((buffer-name (if (functionp ,menu-buffer) (funcall ,menu-buffer) ,menu-buffer)))
           (or (get-buffer buffer-name)
               (with-current-buffer (get-buffer-create buffer-name)
                 (funcall (function ,mode))
                 (tabulated-list-print)))
           (switch-to-buffer buffer-name))))))

;;; Filter
(defun pivotal-read-project-label (prompt &optional project-id)
  "Read label id with PROMPT from the available ones for PROJECT-ID."
  (let* ((project-id (setq pivotal-project-id (or project-id pivotal-project-id (pivotal-read-project-id "Project: "))))
         (entries (setq pivotal-project-labels (or pivotal-project-labels
                                                   (seq-map (lambda (label)
                                                              (let-alist label (cons .name (pivotal-as-string .id))))
                                                            (pivotal-request "GET" (format "/projects/%s/labels" project-id) '(("fields" "id,name"))))))))
    (assoc-default (completing-read prompt entries nil t) entries)))

(defun pivotal-filter-story-by-label (label)
  "Push filter by story LABEL."
  (interactive (list (pivotal-read-project-label "Story label: ")))
  (add-to-list 'pivotal-filter-story (concat "label:" label))
  (and (derived-mode-p 'pivotal-stories-mode) (revert-buffer)))

(defun pivotal-filter-story-by-type (type)
  "Push filter by story TYPE."
  (interactive (list (completing-read "Story type: " pivotal-default-story-types nil t)))
  (add-to-list 'pivotal-filter-story (concat "type:" type))
  (and (derived-mode-p 'pivotal-stories-mode) (revert-buffer)))

(defun pivotal-filter-story-by-state (state)
  "Push filter by story STATE."
  (interactive (list (completing-read "Story state: " pivotal-default-story-states nil t)))
  (add-to-list 'pivotal-filter-story (concat "state:" state))
  (and (derived-mode-p 'pivotal-stories-mode) (revert-buffer)))

(defun pivotal-filter-story-edit ()
  "Edit `pivotal-filter-story'."
  (interactive)
  (setq pivotal-filter-story (split-string (read-string "Story filters: " (string-join pivotal-filter-story " "))))
  (and (derived-mode-p 'pivotal-stories-mode) (revert-buffer)))

(defun pivotal-filter-story-negate ()
  "Takes the top filter from stack and negate it."
  (interactive)
  (cl-destructuring-bind (head . tail) pivotal-filter-story
    (setq pivotal-filter-story (cons (pivotal-filter-toggle head) tail)))
  (and (derived-mode-p 'pivotal-stories-mode) (revert-buffer)))

(defvar pivotal-filter-story-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'pivotal-filter-story-negate)
    (define-key map "e" 'pivotal-filter-story-edit)
    (define-key map "t" 'pivotal-filter-story-by-type)
    (define-key map "l" 'pivotal-filter-story-by-label)
    (define-key map "s" 'pivotal-filter-story-by-state)
    map)
  "Keymap used for filter stories.")

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

(defun pivotal-entries-iterations (&optional project-id)
  "Iteration list for pivotal project PROJECT-ID."
  (let ((project-id (setq pivotal-project-id (or project-id pivotal-project-id (pivotal-read-project-id "Project: ")))))
    (seq-map (lambda (iteration)
               (let-alist iteration
                 (list (pivotal-as-string .number)
                       (vector (pivotal-as-string .number)
                               (pivotal-as-string .team_strength)
                               .start
                               .finish))))
             (pivotal-request "GET" (format "/projects/%s/iterations" project-id) '(("fields" "number,team_strength,start,finish,kind"))))))

(defun pivotal-entries-stories (&optional project-id)
  "Stories list for pivotal project PROJECT-ID."
  (let ((project-id (setq pivotal-project-id (or project-id pivotal-project-id (pivotal-read-project-id "Project: ")))))
    (seq-map (lambda (story)
               (let-alist story
                 (list (pivotal-as-string .id)
                       (vector (pivotal-link-button .url (pivotal-as-string .id))
                               .current_state
                               .story_type
                               .name))))
             (pivotal-request "GET" (format "/projects/%s/stories" project-id)
                              `(("fields" "id,name,story_type,current_state,url,labels(name)")
                                ("filter" ,(string-join pivotal-filter-story " ")))))))

(defun pivotal-story--edit (story &optional save-function)
  "Edit pivotal STORY with SAVE-FUNCTION."
  (with-current-buffer (get-buffer-create "*pivotal-story*")
    (pop-to-buffer (current-buffer))
    (let ((inhibit-read-only t)) (erase-buffer))
    (remove-overlays)
    (kill-all-local-variables)
    (widget-insert (propertize (if (pivotal-story-id story) (format "Pivotal story %s\n\n" (pivotal-story-id story)) "New story\n\n") 'face 'font-lock-constant-face))
    (widget-insert (propertize "Name:\n" 'face 'custom-group-subtitle))
    (widget-create 'editable-field
                   :story story
                   :notify (lambda (widget &rest _ignore)
                             (setf (pivotal-story-name (widget-get widget :story)) (widget-value widget)))
                   (pivotal-story-name story))
    (widget-insert "\n")
    (widget-create 'menu-choice
                   :tag "State"
                   :value (pivotal-story-state story)
                   :button-face 'custom-button
                   `(push-button :tag ,(pivotal-story-state story)
                                 :story ,story
                                 :format "%[%t%]\n"
                                 :notify (lambda (widget &rest _ignore)
                                           (setf (pivotal-story-state (widget-get widget :story)) (widget-value widget))))
                   '(item "accepted")
                   '(item "delivered")
                   '(item "finished")
                   '(item "started")
                   '(item "rejected")
                   '(item "planned")
                   '(item "unstarted")
                   '(item "unscheduled"))
    (widget-insert (propertize "\nDescription:\n" 'face 'custom-group-subtitle))
    (widget-create 'text
                   :story story
                   :notify (lambda (widget &rest _ignore)
                             (setf (pivotal-story-description (widget-get widget :story)) (widget-value widget)))
                   (or (pivotal-story-description story) ""))
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :story story
                   :notify (lambda (widget &rest _ignore)
                             (funcall save-function (widget-get widget :story))
                             (bury-buffer)
                             (message "Done."))
                   "Save")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
                             (bury-buffer)
                             (message "Operation canceled."))
                   "Cancel")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)))

;;;###autoload
(defun pivotal-edit-story (story-id &optional project-id)
  "Show pivotal STORY-ID from PROJECT-ID."
  (interactive (let* ((project-id (or pivotal-project-id (pivotal-read-project-id "Project: ")))
                      (story-id (pivotal-read-story-id "Story: " project-id)))
                 (list story-id project-id)))
  (pivotal-story--edit (let-alist (pivotal-request "GET" (format "/projects/%s/stories/%s" project-id story-id)) (pivotal-story-new :id .id :name .name :description .description :state .state :project-id .project_id))
                       (lambda (story)
                         (let-alist (pivotal-request "PUT" (format "/projects/%s/stories/%s" (pivotal-story-project-id story) (pivotal-story-id story)) nil
                                                     (append `((name        . ,(pivotal-story-name story))
                                                               (description . ,(pivotal-story-description story)))
                                                             (and (pivotal-story-state story)  `((current_state . ,(pivotal-story-state story)))))
                                                     'noerror)
                           (if .error
                               (signal 'pivotal-api-error (list .error .general_problem))
                             (message "Updated story %s at %s" .id .updated_at))))))

;;;###autoload
(defun pivotal-add-story (&optional project-id)
  "Create a story in PROJECT-ID."
  (interactive (list (or pivotal-project-id (pivotal-read-project-id "Project: "))))
  (pivotal-story--edit (pivotal-story-new :name "" :description "" :project-id project-id)
                       (lambda (story)
                         (let-alist (pivotal-request "POST" (format "/projects/%s/stories" project-id) nil
                                                     (append `((name        . ,(pivotal-story-name story))
                                                               (description . ,(pivotal-story-description story)))
                                                             (and (pivotal-story-state story)  `((current_state . ,(pivotal-story-state story)))))
                                                     'noerror)
                           (if .error
                               (signal 'pivotal-api-error (list .error .general_problem))
                             (message "Created story %s at %s" .id .created_at))))))

;;;###autoload
(defun pivotal-edit-project (project-id)
  "Show project information PROJECT-ID."
  (interactive (list (pivotal-read-project-id "Project: ")))
  (let-alist (pivotal-request "GET" (format "/projects/%s" project-id))
    (message "[TODO] Show project: %s" .name)))

;; Entry points

;;;###autoload(autoload 'pivotal-list-projects "pivotal")
(pivotal-define-tbl projects
  "List of pivotal projects."
  :menu    pivotal-list-projects
  :columns [("id"        10 t)
            ("version"   10 t)
            ("iteration" 11 t)
            ("name"      10 t)]
  :entries 'pivotal-entries-projects
  :keymap  '(([return] . pivotal-edit-project)
             ("i"      . pivotal-list-iterations)))

;;;###autoload(autoload 'pivotal-list-iterations "pivotal")
(pivotal-define-tbl iterations
  "List of pivotal project iterations."
  :menu    pivotal-list-iterations
  :columns `[("number"    10 t)
             ("strength"  10 t)
             ("start"     21 ,(apply-partially #'pivotal-tabulated-time< 2))
             ("finish"    21 ,(apply-partially #'pivotal-tabulated-time< 3))]
  :entries 'pivotal-entries-iterations)

;;;###autoload(autoload 'pivotal-list-stories "pivotal")
(pivotal-define-tbl stories
  "List of pivotal project stories."
  :menu    pivotal-list-stories
  :columns [("id"    10 t)
            ("state" 15 t)
            ("type"  10 t)
            ("name"  20 t)]
  :entries 'pivotal-entries-stories
  :keymap  '(([return] . pivotal-edit-story)
             ("+"      . pivotal-add-story)
             ("/"      . pivotal-filter-story-map)))

(provide 'pivotal)
;;; pivotal.el ends here
