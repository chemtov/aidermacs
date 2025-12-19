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

(defvar aidermacs-templates--placeholder-regexp
  "{\\([^}]+\\)}"
  "Regular expression to match template placeholders.
Matches text in the format {Prompt-Text}.")

(defvar aidermacs-templates--metadata-separator "---"
  "Separator line that marks the end of template metadata header.")

(defvar aidermacs-templates--metadata-fields '("title" "description" "command")
  "List of supported metadata field names in template headers.")

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
        (content-lines '()))
    (dolist (line lines)
      (cond
       ;; Found separator - switch to content mode
       ((and in-metadata (string= (string-trim line) aidermacs-templates--metadata-separator))
        (setq in-metadata nil))
       ;; In metadata mode - parse field: value
       (in-metadata
        (when (string-match "^\\([a-zA-Z-]+\\):\\s-*\\(.+\\)$" line)
          (let ((field (downcase (match-string 1 line)))
                (value (string-trim (match-string 2 line))))
            (when (member field aidermacs-templates--metadata-fields)
              (push (cons field value) metadata-alist)))))
       ;; In content mode - collect lines
       (t
        (push line content-lines))))
    ;; Return parsed result
    (list :metadata (nreverse metadata-alist)
          :content (string-join (nreverse content-lines) "\n"))))

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
(defun aidermacs-use-template ()
  "Select and use a prompt template.
Prompts for template selection, processes placeholders, and sends
the resulting command to aidermacs.

If the template has metadata with a 'command' field, that command
will be executed first (blocking) before sending the template content."
  (interactive)
  (let* ((templates (aidermacs-templates--list-templates)))
    (if (null templates)
        (message "No templates found in default or user template directories.")
      ;; Set up marginalia annotation function
      (let* ((marginalia-annotator-registry
              (if (boundp 'marginalia-annotator-registry)
                  (cons (cons 'aidermacs-template #'aidermacs-templates--annotate)
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
        ;; Process and send template content
        (let ((processed-text (aidermacs-templates--process-template content)))
          (when (and processed-text (not (string-empty-p (string-trim processed-text))))
            ;; Use the existing aidermacs command sending infrastructure
            (if (fboundp 'aidermacs--send-command)
                (aidermacs--send-command processed-text)
              (error "aidermacs--send-command not available"))))))))

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