;;; aidermacs-templates.el --- Template system for aidermacs -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 1.6
;; Keywords: ai emacs llm aider ai-pair-programming tools templates
;; URL: https://github.com/MatthewZMD/aidermacs
;; SPDX-License-Identifier: Apache-2.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a template system for aidermacs that allows users to create
;; reusable prompt templates with placeholders. Templates are stored as
;; files in a configurable directory and can contain placeholders in the
;; format {Prompt-Text} which will be replaced with user input when the
;; template is used.
;;
;; Features:
;; - Store templates as simple text files
;; - Support for multiple placeholders per template
;; - Interactive placeholder replacement via completing-read
;; - Integration with aidermacs command sending

;;; Code:

(require 'cl-lib)

(defgroup aidermacs-templates nil
  "Template system for aidermacs prompts."
  :group 'aidermacs)

(defcustom aidermacs-user-templates-directory
  (expand-file-name "aidermacs-templates" user-emacs-directory)
  "Directory where user-created aidermacs prompt templates are stored.
This is the primary location for creating and managing personal templates."
  :type 'directory
  :group 'aidermacs-templates)

(defcustom aidermacs-templates-file-extension '(".txt" ".md")
  "File extensions for template files. This can be a list of strings."
  :type '(repeat string)
  :group 'aidermacs-templates)

(defcustom aidermacs-templates-filled-placeholder-face 'highlight
  "Face used to highlight filled placeholders in template editing buffer."
  :type 'face
  :group 'aidermacs-templates)

(defvar aidermacs-templates--placeholder-regexp
  "{\\([^}]+\\)}"
  "Regular expression to match template placeholders.
Matches text in the format {Prompt-Text}.")

(defvar aidermacs-templates--metadata-separator "---"
  "Separator line that marks the end of template metadata header.")

(defvar aidermacs-templates--metadata-fields '("title" "description" "command")
  "List of supported metadata field names in template headers.")

(defvar-local aidermacs-templates--edit-buffer-template-text nil
  "Original template text being edited in the current buffer.")

(defvar-local aidermacs-templates--edit-buffer-replacements nil
  "Alist of placeholder replacements for the current template buffer.")

(defvar-local aidermacs-templates--edit-buffer-callback nil
  "Callback function to execute when template editing is confirmed.")

(defun aidermacs-templates--get-default-directory ()
  "Return the directory where default templates are stored with the package.
Returns nil if the templates directory cannot be located or doesn't exist."
  (let* ((source-file (or load-file-name
                          (buffer-file-name)
                          ;; Try to locate the library if loaded from compiled code
                          (locate-library "aidermacs-templates")))
         (el-file-dir (when source-file (file-name-directory source-file)))
         (templates-dir (when el-file-dir
                          (expand-file-name "templates" el-file-dir))))
    ;; Only return the directory if it actually exists
    (when (and templates-dir (file-directory-p templates-dir))
      templates-dir)))

(defun aidermacs-templates--ensure-directory ()
  "Ensure the user templates directory exists, creating it if necessary."
  (unless (file-exists-p aidermacs-user-templates-directory)
    (make-directory aidermacs-user-templates-directory t)))

(defun aidermacs-templates--list-templates-from-dir (dir)
  "Return a list of templates from DIR.
Returns an alist of (display-name . file-path) pairs."
  (when (and dir (file-directory-p dir))
    (let* ((extensions (if (listp aidermacs-templates-file-extension)
                           aidermacs-templates-file-extension
                         (list aidermacs-templates-file-extension)))
           (regexp (concat "\\("
                           (mapconcat (lambda (ext) (regexp-quote ext))
                                      extensions
                                      "\\|")
                           "\\)$"))
           ;; Get all files (not directories) in the directory
           (files (directory-files dir t regexp))
           ;; Filter to only include regular files, not directories
           (regular-files (cl-remove-if-not #'file-regular-p files))
           (templates (mapcar (lambda (file)
                               (cons (file-name-sans-extension
                                      (file-name-nondirectory file))
                                     file))
                             regular-files)))
      templates)))

(defun aidermacs-templates--list-templates ()
  "Return a list of available template files from default and user directories.
User templates will override default templates with the same name.
Returns an alist of (display-name . file-path) pairs."
  (aidermacs-templates--ensure-directory)
  (let* ((default-dir (aidermacs-templates--get-default-directory))
         (user-dir aidermacs-user-templates-directory)
         (default-templates (aidermacs-templates--list-templates-from-dir default-dir))
         (user-templates (aidermacs-templates--list-templates-from-dir user-dir))
         ;; User templates come first to take precedence
         ;; :from-end t keeps the first occurrence (user) when duplicates exist
         (merged (cl-remove-duplicates (append user-templates default-templates)
                                       :key #'car :test #'string= :from-end t)))
    merged))

(defun aidermacs-templates--read-template (file-path)
  "Read template content from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun aidermacs-templates--parse-metadata (template-text)
  "Parse metadata header from TEMPLATE-TEXT.
Returns a plist with :metadata (alist of field-value pairs) and :content (template body).
If no metadata header is found, returns (:metadata nil :content TEMPLATE-TEXT)."
  (let ((lines (split-string template-text "\n"))
        (metadata-alist '())
        (in-metadata t)
        (found-separator nil)
        (content-lines '()))
    (dolist (line lines)
      (cond
       ;; Found separator - switch to content mode
       ((and in-metadata (string= (string-trim line) aidermacs-templates--metadata-separator))
        (setq in-metadata nil)
        (setq found-separator t))
       ;; In metadata mode - parse field: value
       (in-metadata
        (if (string-match "^\\([a-zA-Z-]+\\):\\s-*\\(.+\\)$" line)
            (let ((field (downcase (match-string 1 line)))
                  (value (string-trim (match-string 2 line))))
              (when (member field aidermacs-templates--metadata-fields)
                (push (cons field value) metadata-alist)))
          ;; Line doesn't match metadata pattern - if we haven't found any metadata yet,
          ;; this means there's no metadata header, so treat entire text as content
          (unless metadata-alist
            (setq in-metadata nil)
            (push line content-lines))))
       ;; In content mode - collect lines
       (t
        (push line content-lines))))
    ;; Return parsed result
    ;; If no separator was found and no metadata was parsed, return original text as content
    (if (and (not found-separator) (null metadata-alist))
        (list :metadata nil :content template-text)
      (list :metadata (nreverse metadata-alist)
            :content (string-join (nreverse content-lines) "\n")))))

(defun aidermacs-templates--get-metadata-field (metadata field)
  "Get FIELD value from METADATA alist.
Returns nil if field is not present."
  (cdr (assoc field metadata)))

(defun aidermacs-templates--execute-command-blocking (command)
  "Execute COMMAND in aidermacs and wait for it to complete.
Returns t on success, nil on failure or timeout."
  (if (not (fboundp 'aidermacs--send-command))
      (progn
        (message "aidermacs--send-command not available")
        nil)
    (let ((success nil)
          (timeout 30) ; 30 second timeout
          (start-time (current-time)))
      ;; Send command with a callback to set success flag
      (aidermacs--send-command command nil t nil
                               (lambda () (setq success t)))
      ;; Wait for completion or timeout
      (while (and (not success)
                  (< (float-time (time-subtract (current-time) start-time)) timeout))
        (accept-process-output nil 0.1))
      success)))

(defun aidermacs-templates--annotate (cand)
  "Annotate template candidate CAND with its description for marginalia.
This function is designed to work with marginalia-mode."
  (when-let* ((templates (aidermacs-templates--list-templates))
              (template-file (cdr (assoc cand templates)))
              (template-text (aidermacs-templates--read-template template-file))
              (parsed (aidermacs-templates--parse-metadata template-text))
              (metadata (plist-get parsed :metadata))
              (description (aidermacs-templates--get-metadata-field metadata "description")))
    (concat " " (propertize description 'face 'marginalia-documentation))))

(defun aidermacs-templates--extract-placeholders (template-text)
  "Extract all placeholders from TEMPLATE-TEXT.
Returns a list of unique placeholder prompts in order of appearance."
  (let ((placeholders '())
        (pos 0))
    (while (string-match aidermacs-templates--placeholder-regexp template-text pos)
      (let ((placeholder (match-string 1 template-text)))
        (unless (member placeholder placeholders)
          (push placeholder placeholders))
        (setq pos (match-end 0))))
    (nreverse placeholders)))

(defun aidermacs-templates--collect-placeholder-values (placeholders)
  "Collect user input for each placeholder in PLACEHOLDERS.
Returns an alist of (placeholder . value) pairs."
  (mapcar (lambda (placeholder)
            (cons placeholder
                  (read-string (format "%s: " placeholder))))
          placeholders))

(defun aidermacs-templates--replace-placeholders (template-text replacements)
  "Replace all placeholders in TEMPLATE-TEXT with values from REPLACEMENTS.
REPLACEMENTS is an alist of (placeholder . value) pairs."
  (let ((result template-text))
    (dolist (replacement replacements)
      (let* ((placeholder (car replacement))
             (value (cdr replacement))
             (pattern (regexp-quote (format "{%s}" placeholder))))
        (setq result (replace-regexp-in-string pattern value result t t))))
    result))

(defun aidermacs-templates--process-template (template-text)
  "Process TEMPLATE-TEXT by replacing all placeholders with user input.
Returns the processed template string ready to be sent as a command."
  (let* ((placeholders (aidermacs-templates--extract-placeholders template-text))
         (replacements (when placeholders
                        (aidermacs-templates--collect-placeholder-values placeholders))))
    (if replacements
        (aidermacs-templates--replace-placeholders template-text replacements)
      template-text)))

;;;###autoload
(defun aidermacs-templates--highlight-filled-placeholders (buffer replacements)
  "Highlight filled placeholders in BUFFER using REPLACEMENTS.
REPLACEMENTS is an alist of (placeholder . value) pairs."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (dolist (replacement replacements)
        (let* ((placeholder (car replacement))
               (value (cdr replacement))
               (pattern (regexp-quote value)))
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put overlay 'face aidermacs-templates-filled-placeholder-face)
              (overlay-put overlay 'aidermacs-template-highlight t))))))))

(defun aidermacs-templates--clear-highlights (buffer)
  "Clear all template placeholder highlights in BUFFER."
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'aidermacs-template-highlight t)))

(defun aidermacs-templates--collect-placeholder-values-interactive (placeholders template-text &optional template-file)
  "Collect user input for PLACEHOLDERS while displaying TEMPLATE-TEXT in a buffer.
Returns an alist of (placeholder . value) pairs.
The template buffer is displayed and updated in real-time as placeholders are filled.
If TEMPLATE-FILE is provided, the buffer's major mode is set based on the file extension."
  (let* ((buffer-name "*Aidermacs Template*")
         (buffer (get-buffer-create buffer-name))
         (replacements '()))
    (with-current-buffer buffer
      (erase-buffer)
      (insert template-text)
      ;; Set major mode based on file extension
      (when template-file
        (let ((ext (file-name-extension template-file)))
          (cond
           ((string= ext "md") (when (fboundp 'markdown-mode) (markdown-mode)))
           ((string= ext "org") (when (fboundp 'org-mode) (org-mode)))
           ((string= ext "txt") (text-mode))
           (t (text-mode)))))
      (goto-char (point-min))
      (setq buffer-read-only t))

    ;; Display the buffer
    (display-buffer buffer '((display-buffer-reuse-window display-buffer-pop-up-window)))

    ;; Collect values for each placeholder
    (unwind-protect
        (dolist (placeholder placeholders)
          (let ((value (read-string (format "%s: " placeholder))))
            (push (cons placeholder value) replacements)
            ;; Update the buffer with the filled value
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward
                          (regexp-quote (format "{%s}" placeholder)) nil t)
                    (replace-match value t t)))
                ;; Highlight the newly filled placeholder
                (aidermacs-templates--highlight-filled-placeholders buffer (list (cons placeholder value)))))))
      ;; Don't kill the buffer here - we'll use it for editing
      nil)
    (nreverse replacements)))

(defun aidermacs-templates--setup-edit-mode (buffer template-text replacements callback)
  "Setup BUFFER for template editing with keybindings.
TEMPLATE-TEXT is the original template, REPLACEMENTS are the filled values,
and CALLBACK is called when the user confirms with C-c C-c."
  (with-current-buffer buffer
    (setq aidermacs-templates--edit-buffer-template-text template-text)
    (setq aidermacs-templates--edit-buffer-replacements replacements)
    (setq aidermacs-templates--edit-buffer-callback callback)
    (setq buffer-read-only nil)
    ;; Create a new keymap if none exists, otherwise copy the current one
    (let ((keymap (if (current-local-map)
                      (copy-keymap (current-local-map))
                    (make-sparse-keymap))))
      (use-local-map keymap))
    (local-set-key (kbd "C-c C-c") #'aidermacs-templates-confirm-and-use)
    (local-set-key (kbd "C-c C-k") #'aidermacs-templates-cancel)
    (local-set-key (kbd "C-c C-n") #'aidermacs-templates-save-as-new)
    (goto-char (point-min))
    (message "Edit template: C-c C-c to use, C-c C-k to cancel, C-c C-n to save as new")))

(defun aidermacs-templates-confirm-and-use ()
  "Confirm the template and send it to aidermacs."
  (interactive)
  (when aidermacs-templates--edit-buffer-callback
    (let ((content (buffer-string)))
      (aidermacs-templates--clear-highlights (current-buffer))
      (funcall aidermacs-templates--edit-buffer-callback content)
      (kill-buffer (current-buffer)))))

(defun aidermacs-templates-cancel ()
  "Cancel template editing and close the buffer."
  (interactive)
  (when (yes-or-no-p "Cancel template editing? ")
    (aidermacs-templates--clear-highlights (current-buffer))
    (kill-buffer (current-buffer))
    (message "Template editing cancelled")))

(defun aidermacs-templates-save-as-new ()
  "Save the current template as a new template file."
  (interactive)
  (aidermacs-templates--ensure-directory)
  (let* ((name (read-string "New template name: "))
         (extension (if (= 1 (length aidermacs-templates-file-extension))
                        (car aidermacs-templates-file-extension)
                      (completing-read "Select file extension: "
                                       aidermacs-templates-file-extension
                                       nil t (car aidermacs-templates-file-extension))))
         (base-path (expand-file-name
                     (concat name extension)
                     aidermacs-user-templates-directory))
         ;; Use make-temp-name style collision detection
         (file-path (if (file-exists-p base-path)
                        (let ((counter 1))
                          (while (file-exists-p
                                  (expand-file-name
                                   (format "%s-%d%s" name counter extension)
                                   aidermacs-user-templates-directory))
                            (setq counter (1+ counter)))
                          (expand-file-name
                           (format "%s-%d%s" name counter extension)
                           aidermacs-user-templates-directory))
                      base-path))
         (content (buffer-string)))
    (when (and name (not (string-empty-p name)))
      (with-temp-file file-path
        (insert content))
      (message "Template saved as '%s' at %s" (file-name-base file-path) file-path)
      (aidermacs-templates--clear-highlights (current-buffer))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun aidermacs-use-template ()
  "Select and use a prompt template.
Prompts for template selection, processes placeholders, and sends
the resulting command to aidermacs.

If the template has metadata with a 'command' field, that command
will be executed first (blocking) before sending the template content.

The template is displayed in a buffer during placeholder collection,
with filled placeholders highlighted. After all placeholders are filled,
the user can edit the template and use C-c C-c to send it, C-c C-k to
cancel, or C-c C-n to save it as a new template."
  (interactive)
  (let* ((templates (aidermacs-templates--list-templates)))
    (if (null templates)
        (message "No templates found in default or user template directories.")
      ;; Set up marginalia annotation function
      (let* ((marginalia-annotator-registry
              (if (boundp 'marginalia-annotator-registry)
                  (cons (list 'aidermacs-template #'aidermacs-templates--annotate)
                        marginalia-annotator-registry)
                nil))
             (template-name (completing-read "Select template: "
                                            (lambda (string pred action)
                                              (if (eq action 'metadata)
                                                  '(metadata (category . aidermacs-template))
                                                (complete-with-action action
                                                                     (mapcar #'car templates)
                                                                     string pred)))
                                            nil t))
             (template-file (cdr (assoc template-name templates)))
             (template-text (aidermacs-templates--read-template template-file))
             (parsed (aidermacs-templates--parse-metadata template-text))
             (metadata (plist-get parsed :metadata))
             (content (plist-get parsed :content))
             (command (aidermacs-templates--get-metadata-field metadata "command")))
        ;; Execute command if specified
        (when (and command (not (string-empty-p command)))
          (message "Executing template command: %s" command)
          (unless (aidermacs-templates--execute-command-blocking command)
            (error "Failed to execute template command: %s" command)))
        ;; Extract placeholders and collect values interactively
        (let* ((placeholders (aidermacs-templates--extract-placeholders content))
               (buffer-name "*Aidermacs Template*")
               (buffer (get-buffer-create buffer-name)))
          (if (null placeholders)
              ;; No placeholders - just display and allow editing
              (progn
                (with-current-buffer buffer
                  (erase-buffer)
                  (insert content)
                  (goto-char (point-min)))
                (display-buffer buffer '((display-buffer-reuse-window display-buffer-pop-up-window)))
                (aidermacs-templates--setup-edit-mode
                 buffer content '()
                 (lambda (processed-text)
                   (when (and processed-text (not (string-empty-p (string-trim processed-text))))
                     (if (fboundp 'aidermacs--send-command)
                         (aidermacs--send-command processed-text)
                       (error "aidermacs--send-command not available"))))))
            ;; Has placeholders - collect values interactively
            (let ((replacements (aidermacs-templates--collect-placeholder-values-interactive
                                placeholders content template-file)))
              ;; Setup edit mode with callback to send the final text
              (aidermacs-templates--setup-edit-mode
               buffer content replacements
               (lambda (processed-text)
                 (when (and processed-text (not (string-empty-p (string-trim processed-text))))
                   (if (fboundp 'aidermacs--send-command)
                       (aidermacs--send-command processed-text)
                     (error "aidermacs--send-command not available"))))))))))))

;;;###autoload
(defun aidermacs-create-template ()
  "Create a new prompt template interactively.
Prompts for template name and content, then saves it to the templates directory."
  (interactive)
  (aidermacs-templates--ensure-directory)
  (let* ((name (read-string "Template name: "))
         (content (read-string "Template content (use {Placeholder} for inputs): "))
         (extension (if (= 1 (length aidermacs-templates-file-extension))
                        (car aidermacs-templates-file-extension)
                      (completing-read "Select file extension: "
                                       aidermacs-templates-file-extension
                                       nil t (car aidermacs-templates-file-extension))))
         (file-path (expand-file-name
                     (concat name extension)
                     aidermacs-user-templates-directory)))
    (when (and name content extension (not (string-empty-p name)) (not (string-empty-p content)))
      (with-temp-file file-path
        (insert content))
      (message "Template '%s' created at %s" name file-path))))

;;;###autoload
(defun aidermacs-edit-template ()
  "Edit an existing template."
  (interactive)
  (let* ((templates (aidermacs-templates--list-templates)))
    (if (null templates)
        (message "No templates found in default or user template directories.")
      (let* ((template-name (completing-read "Select template to edit: "
                                            (mapcar #'car templates)
                                            nil t))
             (template-file (cdr (assoc template-name templates))))
        (find-file template-file)))))

;;;###autoload
(defun aidermacs-delete-template ()
  "Delete a template."
  (interactive)
  (let* ((templates (aidermacs-templates--list-templates)))
    (if (null templates)
        (message "No templates found in default or user template directories.")
      (let* ((template-name (completing-read "Select template to delete: "
                                            (mapcar #'car templates)
                                            nil t))
             (template-file (cdr (assoc template-name templates))))
        (when (yes-or-no-p (format "Delete template '%s'? " template-name))
          (delete-file template-file)
          (message "Template '%s' deleted" template-name))))))

;;;###autoload
(defun aidermacs-open-templates-directory ()
  "Open the user templates directory in Dired."
  (interactive)
  (aidermacs-templates--ensure-directory)
  (dired aidermacs-user-templates-directory))

(defun aidermacs-templates-diagnose ()
  "Diagnose template system configuration and show what templates are found.
This is useful for debugging template discovery issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Aidermacs Templates Diagnostic*")
    (erase-buffer)
    (insert "=== Aidermacs Templates Diagnostic ===\n\n")

    ;; User templates directory
    (insert (format "User templates directory: %s\n" aidermacs-user-templates-directory))
    (insert (format "  Exists: %s\n" (file-directory-p aidermacs-user-templates-directory)))
    (when (file-directory-p aidermacs-user-templates-directory)
      (let ((user-templates (aidermacs-templates--list-templates-from-dir
                             aidermacs-user-templates-directory)))
        (insert (format "  Found %d user templates: %s\n\n"
                        (length user-templates)
                        (mapcar #'car user-templates)))))

    ;; Default templates directory
    (let ((default-dir (aidermacs-templates--get-default-directory)))
      (insert (format "Default templates directory: %s\n" (or default-dir "nil")))
      (insert (format "  Exists: %s\n" (and default-dir (file-directory-p default-dir))))
      (when default-dir
        (let ((default-templates (aidermacs-templates--list-templates-from-dir default-dir)))
          (insert (format "  Found %d default templates: %s\n\n"
                          (length default-templates)
                          (mapcar #'car default-templates))))))

    ;; Merged templates
    (let ((all-templates (aidermacs-templates--list-templates)))
      (insert (format "All templates (merged): %s\n" (mapcar #'car all-templates)))
      (insert (format "Total template count: %d\n\n" (length all-templates))))

    ;; File extensions
    (insert (format "Template file extensions: %s\n" aidermacs-templates-file-extension))

    ;; Load context
    (insert (format "\nLoad context:\n"))
    (insert (format "  load-file-name: %s\n" load-file-name))
    (insert (format "  buffer-file-name: %s\n" (buffer-file-name)))
    (insert (format "  locate-library result: %s\n" (locate-library "aidermacs-templates")))

    (goto-char (point-min))
    (special-mode)
    (display-buffer (current-buffer))))

(provide 'aidermacs-templates)
;;; aidermacs-templates.el ends here