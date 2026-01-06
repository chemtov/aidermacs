;;; aidermacs-backend-eat.el --- Eat backend for aidermacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Aidermacs Contributors
;; Version: 1.0
;; Keywords: ai emacs llm aider terminal
;; URL: https://github.com/MatthewZMD/aidermacs
;; Package-Requires: ((emacs "28.1") (eat "0.9"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Eat backend for Aidermacs, providing a modern terminal emulator
;; interface for interacting with the Aider process.
;;
;; Features:
;; - Full terminal emulation via Eat
;; - Real-time output formatting with ANSI colors
;; - Integrated formatter for aider-ce linear output
;; - Process management and cleanup
;; - Navigation between AI responses and user prompts
;;
;; The Eat backend provides better terminal compatibility than comint
;; and is lighter weight than vterm while still offering rich formatting.

;;; Code:

(require 'eat)
(require 'aidermacs-backend-eat-formatter)
(require 'eat-osc133-content)

;; Forward declarations
(declare-function aidermacs--prepare-for-code-edit "aidermacs-output")
(declare-function aidermacs--process-message-if-multi-line "aidermacs")
(declare-function aidermacs--command-may-edit-files "aidermacs")
(declare-function aidermacs--store-output "aidermacs-output")
(declare-function aidermacs--is-aidermacs-buffer-p "aidermacs-backends")
(declare-function aidermacs--parse-output-for-files "aidermacs-output")
(declare-function aidermacs--show-ediff-for-edited-files "aidermacs-output")
(declare-function aidermacs--cleanup-temp-buffers "aidermacs-output")
(declare-function aidermacs--detect-edited-files "aidermacs-output")

(defvar aidermacs--last-command)
(defvar aidermacs-prompt-regexp)

(defgroup aidermacs-backend-eat nil
  "Eat backend for Aidermacs."
  :group 'aidermacs)

;;; Buffer-local variables

(defvar-local aidermacs-eat--process nil
  "The Eat process for this buffer.")


(defvar-local aidermacs-eat--ready nil
  "Whether the Eat process is ready for new commands.")

(defvar-local aidermacs-eat--last-formatted-point nil
  "Last point where formatting was applied.")

(defvar-local aidermacs-eat--formatter-state nil
  "Buffer-local state machine for the Eat formatter.")

(defvar-local aidermacs-eat--osc133-enhanced nil
  "Whether OSC 133 enhanced formatting is enabled.")

;;; Live Formatting

(defvar-local aidermacs-eat--needs-formatting nil
  "Flag indicating buffer needs formatting.")

(defvar-local aidermacs-eat--format-start nil
  "Start position for formatting region.")

(defvar-local aidermacs-eat--format-end nil
  "End position for formatting region.")

(defvar-local aidermacs-eat--format-timer nil
  "Timer for periodic formatting checks.")

(defvar-local aidermacs-eat--backup-timer nil
  "Backup timer for catching missed formatting updates.")

;;; Debugging System

(defvar aidermacs-eat--debug-enabled nil
  "Whether debugging is enabled for Eat backend.")

(defvar aidermacs-eat--debug-buffer-name "*aidermacs-eat-debug*"
  "Name of the debug buffer.")

(defun aidermacs-eat-toggle-debug ()
  "Toggle debugging for Eat backend.
When enabled, creates a dedicated debug buffer showing formatting activity."
  (interactive)
  (setq aidermacs-eat--debug-enabled (not aidermacs-eat--debug-enabled))
  (if aidermacs-eat--debug-enabled
      (progn
        (aidermacs-eat--setup-debug-buffer)
        (message "Aidermacs Eat debugging enabled. See buffer: %s" 
                 aidermacs-eat--debug-buffer-name))
    (progn
      (when (get-buffer aidermacs-eat--debug-buffer-name)
        (kill-buffer aidermacs-eat--debug-buffer-name))
      (message "Aidermacs Eat debugging disabled"))))

(defun aidermacs-eat--setup-debug-buffer ()
  "Set up the debug buffer with proper formatting."
  (let ((buffer (get-buffer-create aidermacs-eat--debug-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Aidermacs Eat Backend Debug Log ===\n")
      (insert (format "Started at: %s\n\n" (current-time-string)))
      (goto-char (point-max)))
    buffer))

(defun aidermacs-eat--debug-log (format-string &rest args)
  "Log a debug message to the debug buffer if debugging is enabled.
FORMAT-STRING and ARGS work like `format'."
  (when aidermacs-eat--debug-enabled
    (let ((message (apply #'format format-string args))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (when-let ((buffer (get-buffer aidermacs-eat--debug-buffer-name)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert (format "[%s] %s\n" timestamp message))
            ;; Keep buffer size reasonable (last 1000 lines)
            (when (> (count-lines (point-min) (point-max)) 1000)
              (goto-char (point-min))
              (forward-line 200)
              (delete-region (point-min) (point)))))))))

(defun aidermacs-eat--debug-show-content (label content &optional max-length)
  "Show CONTENT in debug log with LABEL.
MAX-LENGTH limits how much content to show (default 100)."
  (when aidermacs-eat--debug-enabled
    (let* ((max-len (or max-length 100))
           (truncated (if (> (length content) max-len)
                         (concat (substring content 0 max-len) "...")
                       content)))
      ;; Use %S format which safely escapes the string
      (aidermacs-eat--debug-log "%s: %S" label truncated))))

(defun aidermacs-eat--debug-show-properties (content start end)
  "Show text properties in CONTENT from START to END."
  (when aidermacs-eat--debug-enabled
    (let ((props-found nil))
      (dotimes (i (min 10 (- end start))) ; Check first 10 chars
        (let ((pos (+ start i)))
          (when (< pos (length content))
            (let ((props (text-properties-at pos content)))
              (when props
                (push (cons pos props) props-found))))))
      (when props-found
        (aidermacs-eat--debug-log "Text properties found: %S" props-found)))))

(defun aidermacs-eat--setup-osc133-formatting ()
  "Set up OSC 133 enhanced formatting for aidermacs."
  (when (eat-osc133-content-enable #'aidermacs-eat--format-region)
    (setq-local aidermacs-eat--osc133-enhanced t)
    
    ;; Add table formatting hook to command completion
    (add-hook 'eat-shell-command-end-hook #'aidermacs-eat--format-tables-after-command nil t)
    
    (aidermacs-eat--debug-log "OSC 133 enhanced formatting enabled with table post-processing")
    t))

(defun aidermacs-eat--get-last-prompt-position ()
  "Get the position of the end of the last prompt."
  (save-excursion
    (goto-char (point-max))
    (let ((regex (if (and (boundp 'eat-shell-prompt-regexp)
                          eat-shell-prompt-regexp)
                     eat-shell-prompt-regexp
                   ;; Fallback regex matching setup-prompt-detection
                   (concat "\\(?:"
                           "^[^[:space:]<]*>[[:space:]]*$"
                           "\\|^architect>[[:space:]]*$"
                           "\\|^ask>[[:space:]]*$"
                           "\\|^help>[[:space:]]*$"
                           "\\)"))))
      (when (re-search-backward regex nil t)
        (match-end 0)))))

(defun aidermacs-eat--on-update ()
  "Scan and format the rendered buffer content after Eat updates.
Only used as fallback when OSC 133 is not available."
  (when (and (aidermacs--is-aidermacs-buffer-p)
             aidermacs-eat--formatter-state
             (not aidermacs-eat--osc133-enhanced))
    (let ((inhibit-read-only t)) ; Allow modifications to read-only buffer
      (aidermacs-eat--scan-and-format-buffer))))

(defun aidermacs-eat--backup-scanner (buffer)
  "Backup scanner to catch any missed formatting updates.
Only used as fallback when OSC 133 is not available."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (aidermacs--is-aidermacs-buffer-p)
                    aidermacs-eat--formatter-state
                    (not aidermacs-eat--osc133-enhanced))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (aidermacs-eat--scan-and-format-buffer)))))

(defun aidermacs-eat--scan-and-format-buffer ()
  "Scan the buffer and apply formatting with better point tracking.
Only used as fallback when OSC 133 is not available."
  (let* ((last-prompt (aidermacs-eat--get-last-prompt-position))
         (end-point (point-max))
         (current-formatted (or aidermacs-eat--last-formatted-point (point-min)))
         (start-point current-formatted))
    
    ;; Handle buffer shrinking (e.g. backspace, clear)
    (when (> start-point end-point)
      (setq start-point end-point)
      (setq-local aidermacs-eat--last-formatted-point end-point))
    
    ;; Logic to separate User Input from Streaming Output
    ;; If we are on the same line as the prompt, it is User Input -> Stop scanning at prompt.
    ;; If we are on a different line, it is Streaming Output -> Scan everything.
    (when (and last-prompt (> end-point last-prompt))
      (save-excursion
        (goto-char last-prompt)
        ;; Check if end-point is within the same line as prompt
        (when (>= (line-end-position) end-point)
          ;; Same line: Ignore user input to prevent frenzy loops
          (setq end-point last-prompt))))

    (when (> end-point start-point)
      (aidermacs-eat--debug-log "Scanner state: last-formatted=%s last-prompt=%s point-max=%d"
                               aidermacs-eat--last-formatted-point last-prompt (point-max))
      (aidermacs-eat--debug-log "Scanning buffer from %d to %d (%d new chars)" 
                               start-point end-point (- end-point start-point))
      (aidermacs-eat--format-region start-point end-point)
      (setq-local aidermacs-eat--last-formatted-point end-point))))

(defun aidermacs-eat--format-region (start end)
  "Format the region from START to END - called by OSC 133 framework or fallback scanner."
  (aidermacs-eat--debug-log "Formatting region %d-%d (%d chars) [OSC133: %s]" 
                           start end (- end start) 
                           (if aidermacs-eat--osc133-enhanced "yes" "no"))
  
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-start (line-beginning-position))
               (line-end (min (line-end-position) end))
               (line-content (buffer-substring-no-properties line-start line-end)))
          (when (> (length (string-trim line-content)) 0)
            (aidermacs-eat--format-line line-start line-end line-content))
          (forward-line 1))))))

(defun aidermacs-eat--format-line (start end content)
  "Apply ALL applicable formatting to a single line from START to END with CONTENT."
  (let ((trimmed (string-trim content))
        (applied-primary nil)
        (context (aidermacs-eat--detect-current-block-context start))
        (search-replace-context (aidermacs-eat--is-inside-search-replace-block-p start)))
    
    ;; DEBUG: Log context
    (aidermacs-eat--debug-log "format-line context check: %S inside-search: %S" context search-replace-context)
    
    ;; Handle different contexts
    (cond
     ;; Inside code block - context is now (code-block type start-pos)
     ((and (listp context) 
           (eq (car context) 'code-block))
      (let ((block-type (nth 1 context))
            (block-start (nth 2 context)))
        ;; Only process if not already processed
        (unless (get-text-property start 'aidermacs-syntax-applied)
          (aidermacs-eat--debug-log "Formatting code block line (%s): %s" block-type trimmed)
          (aidermacs-eat--format-multi-line-code-block block-start block-type)
          (aidermacs-eat--mark-code-block-as-processed block-start))
        
        (setq applied-primary 'code-block)))
     
     ;; Inside SEARCH/REPLACE block - apply appropriate formatting
     ((memq search-replace-context '(search-block replace-block))
      (aidermacs-eat--debug-log "Formatting %s line: %s" search-replace-context trimmed)
      (aidermacs-eat--format-search-replace-block-line start end content search-replace-context)
      (setq applied-primary search-replace-context))
     
     ;; Normal context - apply primary formatting (background/major styling) - only one per line
     (t
      (cond
       ;; SEARCH/REPLACE blocks (highest priority)
       ((aidermacs-eat--is-search-marker-p trimmed)
        (aidermacs-eat--debug-log "Formatting SEARCH marker: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-search-block-face)
        (aidermacs-eat--mark-position start 'search-replace)
        (setq applied-primary 'search-marker))
       
       ((aidermacs-eat--is-diff-marker-p trimmed)
        (aidermacs-eat--debug-log "Formatting diff separator: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-search-block-face)
        (setq applied-primary 'diff-marker))
       
       ((aidermacs-eat--is-replace-marker-p trimmed)
        (aidermacs-eat--debug-log "Formatting REPLACE marker: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-replace-block-face)
        (setq applied-primary 'replace-marker))
       
       ;; Tool calls (high priority)
       ((aidermacs-eat--is-tool-call-line-p content)
        (aidermacs-eat--debug-log "Formatting tool call: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-tool-call-face)
        (aidermacs-eat--mark-position start 'ai-response)
        (setq applied-primary 'tool-call))
       
       ;; AI reasoning blocks
       ((aidermacs-eat--is-thinking-marker-p content)
        (aidermacs-eat--debug-log "Formatting thinking marker: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-thinking-face)
        (aidermacs-eat--mark-position start 'ai-response)
        (setq applied-primary 'thinking))
       
       ((aidermacs-eat--is-answer-marker-p content)
        (aidermacs-eat--debug-log "Formatting answer marker: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-answer-face)
        (aidermacs-eat--mark-position start 'ai-response)
        (setq applied-primary 'answer))
       
       ;; Interactive prompts
       ((aidermacs-eat--is-interactive-prompt-p content)
        (aidermacs-eat--debug-log "Formatting interactive prompt: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-prompt-face)
        (aidermacs-eat--mark-position start 'user-prompt)
        (setq applied-primary 'prompt))
       
       ;; Tool results
       ((aidermacs-eat--is-tool-success-p content)
        (aidermacs-eat--debug-log "Formatting tool success: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-success-face)
        (setq applied-primary 'success))
       
       ((aidermacs-eat--is-tool-error-p content)
        (aidermacs-eat--debug-log "Formatting tool error: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-error-face)
        (setq applied-primary 'error))
       
       ((aidermacs-eat--is-tool-warning-p content)
        (aidermacs-eat--debug-log "Formatting tool warning: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-warning-face)
        (setq applied-primary 'warning))
       
       ;; Context blocks
       ((aidermacs-eat--is-context-start-p content)
        (aidermacs-eat--debug-log "Formatting context start: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-context-face)
        (setq applied-primary 'context-start))
       
       ((aidermacs-eat--is-context-end-p content)
        (aidermacs-eat--debug-log "Formatting context end: %s" trimmed)
        (put-text-property start end 'face 'aidermacs-eat-context-face)
        (setq applied-primary 'context-end))
       
       ;; Markdown headers (only if not already formatted)
       ((and (aidermacs-eat--is-markdown-header-p content)
             (not (aidermacs-eat--is-tool-call-line-p content)))
        (aidermacs-eat--debug-log "Formatting markdown header: %s" trimmed)
        (let* ((level (aidermacs-eat--get-header-level content))
               (face (aidermacs-eat--get-header-face level)))
          (put-text-property start end 'face face)
          
          ;; Hide leading hashes and spaces
          (when (string-match "^\\([[:space:]]*#+[[:space:]]*\\)" content)
            (put-text-property (+ start (match-beginning 1)) 
                               (+ start (match-end 1)) 
                               'invisible t))
          
          ;; Hide wrapping bold markers if present (e.g. ### **Title**)
          (when (string-match "^\\(?:[[:space:]]*#+[[:space:]]*\\)\\(\\*\\*\\).+\\(\\*\\*\\)[[:space:]]*$" content)
            (put-text-property (+ start (match-beginning 1)) 
                               (+ start (match-end 1)) 
                               'invisible t)
            (put-text-property (+ start (match-beginning 2)) 
                               (+ start (match-end 2)) 
                               'invisible t)))
        (setq applied-primary 'markdown-header))
       
       ((and (aidermacs-eat--is-markdown-code-block-p content)
             ;; Don't treat ```diff as a regular code block marker
             (not (string-match-p "^```diff$" (string-trim content))))
        (aidermacs-eat--debug-log "Formatting markdown code block: %s" trimmed)
        ;; Hide the fence line completely (including newline)
        (put-text-property start (min (point-max) (1+ end)) 'invisible t)
        (setq applied-primary 'code-block)))))
    
    ;; ALWAYS apply inline formatting (function calls, markdown bold) regardless of primary formatting
    ;; But only if we're not inside a code block (where we want to preserve literal content)
    (if (and (listp context) 
                 (eq (car context) 'code-block))
        (aidermacs-eat--debug-log "Skipping inline formatting due to code block context")
      (aidermacs-eat--format-inline-elements start end content applied-primary))))

(defun aidermacs-eat--format-inline-elements (start end content applied-primary)
  "Apply inline formatting like function calls, markdown bold, and inline code to region.
APPLIED-PRIMARY indicates what primary formatting was already applied.
This function applies ALL complete inline patterns found in the content."
  ;; Debug log to verify we are attempting formatting
  (aidermacs-eat--debug-log "format-inline-elements START (primary=%s) range=%d-%d content='%s'" 
                           applied-primary start end (string-trim content))

  ;; Function calls - apply unless it conflicts with tool calls
  (unless (eq applied-primary 'tool-call)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\(\\w+:\\w+([^)]*)\\)" end t)
        (aidermacs-eat--debug-log "Applied function call face at %d" (match-beginning 0))
        (put-text-property (match-beginning 1) (match-end 1) 
                          'face 'aidermacs-eat-function-call-face))))
  
  ;; Markdown Links [Label](URL) - allow newlines
  (unless (eq applied-primary 'tool-call)
    (save-excursion
      (goto-char start)
      (aidermacs-eat--debug-log "Scanning for LINKS in %d-%d" start end)
      ;; Regex explanation:
      ;; 1. \[ starts the label
      ;; 2. \([^]]+\) Captures label content (Group 1)
      ;; 3. \] ( starts the url
      ;; 4. \([^)]+\) Captures URL content (Group 2) - allows newlines
      ;; 5. ) ends the link
      (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" end t)
        (let* ((label-start (match-beginning 1))
               (label-end (match-end 1))
               (url-raw (match-string-no-properties 2))
               ;; Clean up newlines/spaces from the URL
               (url (replace-regexp-in-string "[ \t\n\r]+" "" url-raw)))
          
          (aidermacs-eat--debug-log "Applied link button for %s -> %s" (match-string 1) url)
          
          ;; Hide the brackets and parenthesis parts
          (put-text-property (match-beginning 0) label-start 'invisible t)   ; [
          (put-text-property label-end (match-beginning 2) 'invisible t)     ; ](
          (put-text-property (match-end 2) (match-end 0) 'invisible t)       ; )
          
          ;; Make the label a button
          (make-text-button label-start label-end
                            'action #'aidermacs-eat--open-link-button
                            'url url
                            'help-echo (format "Open Link: %s" url)
                            'follow-link t
                            'face '(:inherit link :underline t))))))
  
  ;; Markdown bold - allow in headers, but not tool calls
  (unless (eq applied-primary 'tool-call)
    (save-excursion
      (goto-char start)
      (aidermacs-eat--debug-log "Scanning for BOLD in %d-%d" start end)
      ;; Use capture groups to hide markers: Group 1=**, Group 2=content, Group 3=**
      (while (re-search-forward "\\(\\*\\*\\)\\([^*]+\\)\\(\\*\\*\\)" end t)
        (aidermacs-eat--debug-log "Applied bold face at %d-%d (content: '%s')" 
                                 (match-beginning 0) (match-end 0) (match-string 2))
        (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
        (put-text-property (match-beginning 2) (match-end 2) 
                          'face 'aidermacs-eat-markdown-bold-face)
        (put-text-property (match-beginning 3) (match-end 3) 'invisible t))))
  
  ;; Inline code - allow in headers, but not tool calls
  (unless (eq applied-primary 'tool-call)
    (save-excursion
      (goto-char start)
      (aidermacs-eat--debug-log "Scanning for INLINE CODE in %d-%d" start end)
      ;; Use capture groups to hide markers: Group 1=`, Group 2=content, Group 3=`
      (while (re-search-forward "\\(`\\)\\([^`]+\\)\\(`\\)" end t)
        (aidermacs-eat--debug-log "Applied inline code face at %d-%d (content: '%s')" 
                                 (match-beginning 0) (match-end 0) (match-string 2))
        (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
        (put-text-property (match-beginning 2) (match-end 2) 
                          'face 'aidermacs-eat-markdown-code-face)
        (put-text-property (match-beginning 3) (match-end 3) 'invisible t)))))

(defun aidermacs-eat--maybe-highlight-header-before-align ()
  "Optionally highlight table header before alignment."
  ;; This is a placeholder for potential header highlighting
  ;; Could be expanded to detect and highlight header rows
  nil)

(defun aidermacs-eat--format-tables-after-command (&optional _command)
  "Format tables in the last command output using markdown-table-align.
Optional _COMMAND argument is ignored but allows this function to be used as a hook."
  (when (and (boundp 'eat-osc133-content--command-start-point)
             eat-osc133-content--command-start-point)
    
    (let ((command-start eat-osc133-content--command-start-point)
          (command-end (point-max)))
      
      (aidermacs-eat--debug-log "Formatting tables in command output %d-%d" 
                               command-start command-end)
      
      (save-excursion
        (save-window-excursion  ; Don't move cursor
          (let ((inhibit-read-only t))
            
            ;; Go through the command output and format any tables
            (goto-char command-start)
            (while (re-search-forward "^|.*|$" command-end t)
              (beginning-of-line)
              
              ;; Optional: Add header highlighting before alignment
              (aidermacs-eat--maybe-highlight-header-before-align)
              
              ;; Use markdown-table-align
              (condition-case err
                  (progn
                    (unless (fboundp 'markdown-table-align)
                      (require 'markdown-mode))
                    (markdown-table-align)
                    (aidermacs-eat--debug-log "Aligned table at %d" (point)))
                (error
                 (aidermacs-eat--debug-log "Error aligning table: %s" 
                                          (error-message-string err))))
              
              ;; Move past the ENTIRE table (all |...| lines)
              (while (and (not (eobp))
                          (< (point) command-end)
                          (looking-at "^|.*|$"))
                (forward-line 1)))))))))

(defun aidermacs-eat--open-link-button (button)
  "Action function for aidermacs link buttons."
  (let ((url (button-get button 'url)))
    (when url
      (browse-url url))))

(defun aidermacs-eat--mark-position (pos type)
  "Mark position POS with marker TYPE for navigation."
  (put-text-property pos (min (1+ pos) (point-max)) 'aidermacs-marker t)
  (put-text-property pos (min (1+ pos) (point-max)) 'aidermacs-marker-type type)
  (aidermacs-eat--debug-log "Marked position %d as %s" pos type))

(defun aidermacs-eat--mark-code-block-as-processed (block-start)
  "Mark a code block starting at BLOCK-START as having syntax highlighting applied."
  (save-excursion
    (goto-char block-start)
    (forward-line 1) ; Skip the ```language line
    (let ((continue t))
      (while (and continue (not (eobp)))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line-content (buffer-substring-no-properties line-start line-end)))
          ;; Check for closing fence - any line starting with ``` ends the block
          (if (string-prefix-p "```" (string-trim line-content))
              (setq continue nil)
            ;; Not a fence, mark as processed
            (put-text-property line-start line-end 'aidermacs-syntax-applied t)
            (aidermacs-eat--debug-log "Marked line as syntax-applied: %s" line-content)
            (forward-line 1)))))))

(defun aidermacs-eat--clear-syntax-cache ()
  "Clear the syntax highlighting cache."
  (interactive)
  (clrhash aidermacs-eat--syntax-cache)
  (message "Syntax highlighting cache cleared"))

(defun aidermacs-eat-debug-osc133-state ()
  "Debug OSC 133 enhanced formatting state."
  (interactive)
  (message "=== Aidermacs OSC 133 Debug ===")
  (message "OSC 133 enhanced: %s" aidermacs-eat--osc133-enhanced)
  (when (fboundp 'eat-osc133-content-enabled-p)
    (message "OSC 133 framework enabled: %s" (eat-osc133-content-enabled-p)))
  (when (fboundp 'eat-osc133-content-debug-state)
    (eat-osc133-content-debug-state)))

(defun aidermacs-eat--check-for-prompt-completion ()
  "Check for prompt completion using existing formatter logic."
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-match-p aidermacs-prompt-regexp content)
      (setq-local aidermacs--ready t)
      (aidermacs--store-output content)
      
      ;; Handle file edits
      (let ((edited-files (aidermacs--detect-edited-files)))
        (if edited-files
            (aidermacs--show-ediff-for-edited-files edited-files)
          (aidermacs--cleanup-temp-buffers))))))

(defun aidermacs-eat--sentinel (process event)
  "Sentinel function for Eat PROCESS.
EVENT describes what happened to the process."
  (when (memq (process-status process) '(exit signal))
    (message "Aidermacs Eat process %s: %s" process (string-trim event))))

(defun aidermacs-eat--setup-prompt-detection ()
  "Set up Eat to recognize aider prompts."
  (when (bound-and-true-p eat-term)
    ;; Configure Eat's prompt detection for aider
    (setq-local eat-shell-prompt-regexp
                (concat "\\(?:"
                        "^[^[:space:]<]*>[[:space:]]*$"     ; Basic prompt like "main> "
                        "\\|^architect>[[:space:]]*$"       ; Architect mode
                        "\\|^ask>[[:space:]]*$"             ; Ask mode  
                        "\\|^help>[[:space:]]*$"            ; Help mode
                        "\\)"))
    
    ;; Also set the continuation prompt if needed
    (setq-local eat-shell-continuation-prompt-regexp "^[[:space:]]*")
    
    ;; Enable prompt tracking
    (when (fboundp 'eat-shell-mode)
      (eat-shell-mode 1))))

(defun aidermacs-run-eat (program args buffer-name)
  "Create an Eat-based buffer and run aidermacs program."
  (aidermacs-eat--debug-log "run-eat called: program=%s args=%S buffer=%s" 
                           program args buffer-name)
  
  (let* ((buffer (get-buffer-create buffer-name))
         ;; ONLY add --linear-output, remove invalid flags
         (args (append args (list "--linear-output")))
         (command (mapconcat #'shell-quote-argument (cons program args) " ")))
    
    (aidermacs-eat--debug-log "Final command: %s" command)
    
    (with-current-buffer buffer
      ;; Set up Eat mode first
      (eat-mode)
      (aidermacs-eat--debug-log "Eat mode enabled")
      
      ;; Set up our formatter BEFORE starting process
      (aidermacs-eat-setup-formatter)
      
      ;; Start the process - let Eat handle terminal setup
      (eat-exec buffer "aidermacs-eat" "/bin/sh" nil (list "-c" command))
      (aidermacs-eat--debug-log "eat-exec completed")
      
      ;; Set up prompt detection AFTER process starts
      (aidermacs-eat--setup-prompt-detection)
      (aidermacs-eat--debug-log "Prompt detection configured")
      
      (when-let ((proc (get-buffer-process buffer)))
        (setq-local aidermacs-eat--process proc))
      
      ;; Initialize tracking point
      (setq-local aidermacs-eat--last-formatted-point (point-min))
      
      ;; Try to set up OSC 133 enhanced formatting first
      (if (aidermacs-eat--setup-osc133-formatting)
          (aidermacs-eat--debug-log "Using OSC 133 enhanced formatting")
        ;; Fallback to timer-based approach for non-OSC 133 terminals
        (progn
          (add-hook 'eat-update-hook #'aidermacs-eat--on-update nil t)
          (aidermacs-eat--debug-log "Added eat-update-hook (fallback)")
          
          (setq-local aidermacs-eat--backup-timer 
                      (run-with-timer 0.1 0.5 #'aidermacs-eat--backup-scanner buffer))
          (aidermacs-eat--debug-log "Started backup scanner timer (fallback)")))
      
      ;; Initialize state
      (setq-local aidermacs--ready t)
      (setq-local aidermacs-eat--output-accumulator "")
      (aidermacs-eat--debug-log "Initialized state: ready=%s" aidermacs--ready)
      
      ;; Enable our minor mode
      (aidermacs-eat-mode 1)
      (aidermacs-eat--debug-log "aidermacs-eat-mode enabled"))
    
    (aidermacs-eat--debug-log "run-eat completed, returning buffer")
    buffer))

;;; Command Sending

(defun aidermacs--send-command-eat (buffer command)
  "Send COMMAND using proper Eat input methods."
  (with-current-buffer buffer
    (setq-local aidermacs--ready nil)
    
    ;; Store command for tracking
    (unless (member (downcase command) '("" "y" "n" "d" "yes" "no"))
      (setq aidermacs--last-command command)
      (when (aidermacs--command-may-edit-files command)
        (aidermacs--prepare-for-code-edit)))
    
    ;; Process and send command
    (let ((processed-command (aidermacs--process-message-if-multi-line command)))
      (eat-term-send-string eat-terminal processed-command)
      (eat-term-send-string eat-terminal "\r"))))

(defun aidermacs--send-command-redirect-eat (buffer command)
  "Send COMMAND to Eat BUFFER and collect output.
This is used for commands where we want to capture output without display."
  (with-current-buffer buffer
    (when (and aidermacs-eat--process
               (process-live-p aidermacs-eat--process))
      ;; For Eat, we'll use a temporary accumulator
      (let ((output-buffer (get-buffer-create " *aidermacs-eat-redirect*"))
            (old-filter (process-filter aidermacs-eat--process)))
        (unwind-protect
            (progn
              ;; Set up temporary filter to capture output
              (set-process-filter aidermacs-eat--process
                                 (lambda (_proc output)
                                   (with-current-buffer output-buffer
                                     (goto-char (point-max))
                                     (insert output))))

              ;; Send command
              (process-send-string aidermacs-eat--process
                                  (concat command "\n"))

              ;; Wait for prompt
              (with-current-buffer output-buffer
                (erase-buffer))

              (while (not (with-current-buffer output-buffer
                           (goto-char (point-min))
                           (re-search-forward aidermacs-prompt-regexp nil t)))
                (accept-process-output aidermacs-eat--process 0.1))

              ;; Store the output
              (aidermacs--store-output (with-current-buffer output-buffer
                                        (buffer-string))))

          ;; Restore original filter
          (set-process-filter aidermacs-eat--process old-filter)
          (kill-buffer output-buffer))))))

;;; Interrupt Handling

(defun aidermacs-eat-interrupt-subjob ()
  "Interrupt the current Eat subjob and cleanup temp buffers."
  (interactive)
  (when (and aidermacs-eat--process
             (process-live-p aidermacs-eat--process))
    (interrupt-process aidermacs-eat--process)
    (when (aidermacs--is-aidermacs-buffer-p)
      (aidermacs--cleanup-temp-buffers))))

;;; Cleanup

(defun aidermacs-eat--cleanup ()
  "Clean up Eat backend resources."
  ;; Clean up OSC 133 formatting
  (when aidermacs-eat--osc133-enhanced
    (eat-osc133-content-disable)
    (remove-hook 'eat-shell-command-end-hook #'aidermacs-eat--format-tables-after-command t)
    (setq aidermacs-eat--osc133-enhanced nil))
  
  ;; Clean up fallback timers
  (when aidermacs-eat--backup-timer
    (cancel-timer aidermacs-eat--backup-timer)
    (setq aidermacs-eat--backup-timer nil))
  (when aidermacs-eat--format-timer
    (cancel-timer aidermacs-eat--format-timer)
    (setq aidermacs-eat--format-timer nil))
  
  ;; Clean up terminal and process
  (when (and (boundp 'eat-terminal)
             eat-terminal
             (eat-term-live-p eat-terminal))
    (eat-term-delete eat-terminal))
  (when (and aidermacs-eat--process
             (process-live-p aidermacs-eat--process))
    (delete-process aidermacs-eat--process)))


;;; Minor Mode

(defvar aidermacs-eat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'aidermacs-eat-interrupt-subjob)
    ;; Use Eat's built-in prompt navigation
    (define-key map (kbd "C-M-n") #'eat-next-shell-prompt)
    (define-key map (kbd "C-M-p") #'eat-previous-shell-prompt)
    (define-key map (kbd "C-c C-d") #'aidermacs-eat-toggle-debug)
    ;; Bind formatter navigation keys
    (define-key map (kbd "M-n") #'aidermacs-eat-next-ai-response)
    (define-key map (kbd "M-p") #'aidermacs-eat-previous-ai-response)
    ;; Syntax highlighting cache management
    (define-key map (kbd "C-c C-r") #'aidermacs-eat--clear-syntax-cache)
    ;; OSC 133 debug
    (define-key map (kbd "C-c C-o") #'aidermacs-eat-debug-osc133-state)
    map)
  "Keymap for `aidermacs-eat-mode'.")

(define-minor-mode aidermacs-eat-mode
  "Minor mode for Aidermacs Eat backend.
Provides keybindings and functionality specific to Aidermacs in Eat buffers."
  :lighter " AiderEat"
  :keymap aidermacs-eat-mode-map
  (if aidermacs-eat-mode
      (progn
        (add-hook 'kill-buffer-hook #'aidermacs-eat--cleanup nil t))
    (remove-hook 'kill-buffer-hook #'aidermacs-eat--cleanup t)))

(provide 'aidermacs-backend-eat)

;;; aidermacs-backend-eat.el ends here
