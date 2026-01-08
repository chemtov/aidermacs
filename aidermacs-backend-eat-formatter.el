;;; aidermacs-backend-eat-formatter.el --- Eat output formatter for aider-ce -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Aidermacs Contributors
;; Version: 1.0
;; Keywords: ai emacs llm aider terminal formatting
;; URL: https://github.com/MatthewZMD/aidermacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Real-time output formatter for aider-ce when running in Eat terminal.
;; Transforms aider-ce's linear-mode text output into richly formatted
;; terminal display using ANSI escape sequences.
;;
;; Features:
;; - Tool call block highlighting
;; - SEARCH/REPLACE diff blocks with colored backgrounds
;; - AI reasoning block formatting
;; - Context block highlighting (agent mode)
;; - Interactive prompt detection
;; - Tool result formatting
;;
;; The formatter uses a state machine to handle multi-line blocks correctly.

;;; Code:

(require 'cl-lib)
(require 'eat)

;;; Language to Major Mode Mapping

(defvar aidermacs-eat--language-mode-alist
  '(("diff" . diff-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("elisp" . emacs-lisp-mode)
    ("python" . python-mode)
    ("javascript" . js-mode)
    ("bash" . sh-mode)
    ("shell" . sh-mode)
    ("c" . c-mode)
    ("cpp" . c++-mode)
    ("java" . java-mode)
    ("go" . go-mode)
    ("rust" . rust-mode)
    ("typescript" . typescript-mode)
    ("json" . js-mode)
    ("xml" . nxml-mode)
    ("html" . html-mode)
    ("css" . css-mode)
    ("sql" . sql-mode)
    ("yaml" . yaml-mode)
    ("markdown" . markdown-mode))
  "Mapping from code block language to major mode.")

(defvar aidermacs-eat--syntax-cache (make-hash-table :test 'equal)
  "Cache for syntax highlighting results to improve performance.")

;;; Face Definitions

(defgroup aidermacs-backend-eat nil
  "Eat backend for Aidermacs."
  :group 'aidermacs)

(defface aidermacs-eat-tool-call-face
  '((t (:inherit font-lock-function-name-face :weight bold :background "#252525" :extend t)))
  "Face for tool call blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-search-block-face
  '((t (:inherit diff-removed :extend t)))
  "Face for SEARCH diff blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-replace-block-face
  '((t (:inherit diff-added :extend t)))
  "Face for REPLACE diff blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-thinking-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for AI thinking blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-answer-face
  '((t (:inherit success :weight bold)))
  "Face for AI answer blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-context-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face for context blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-success-face
  '((t (:inherit success :weight bold)))
  "Face for success messages."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-error-face
  '((t (:inherit error :weight bold)))
  "Face for error messages."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-warning-face
  '((t (:inherit warning :weight bold)))
  "Face for warning messages."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-prompt-face
  '((t (:inherit font-lock-builtin-face :weight bold)))
  "Face for interactive prompts."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-markdown-h1-face
  '((t (:inherit outline-1 :height 1.4 :weight bold :underline t)))
  "Face for # headers."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-markdown-h2-face
  '((t (:inherit outline-2 :height 1.3 :weight bold :underline t)))
  "Face for ## headers."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-markdown-h3-face
  '((t (:inherit outline-3 :height 1.2 :weight bold)))
  "Face for ### headers."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-markdown-header-face
  '((t (:inherit outline-4 :height 1.1 :weight bold)))
  "Face for #### and higher level headers."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-markdown-bold-face
  '((t (:weight bold)))
  "Face for markdown bold text."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-markdown-code-face
  '((((class color) (background dark))
     (:inherit font-lock-constant-face :background "#222222" :extend t))
    (((class color) (background light))
     (:inherit font-lock-constant-face :background "#f0f0f0" :extend t))
    (t (:inherit font-lock-constant-face)))
  "Face for markdown code blocks."
  :group 'aidermacs-backend-eat)

(defface aidermacs-eat-function-call-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for function calls like 'module:function()'."
  :group 'aidermacs-backend-eat)

;;; State Tracking

(defvar-local aidermacs-eat--format-state 'normal
  "Current state of the formatter (normal, search, replace).")

(defvar-local aidermacs-eat--scan-point nil
  "Marker pointing to the end of the last formatted region.")

(defun aidermacs-eat--reset-scan-point ()
  "Reset the scan point to the beginning of the buffer."
  (if aidermacs-eat--scan-point
      (set-marker aidermacs-eat--scan-point (point-min))
    (setq aidermacs-eat--scan-point (copy-marker (point-min)))))

;;; Context Detection Functions

(defun aidermacs-eat--detect-current-block-context (pos)
  "Detect what block we're in by scanning backward from POS.
Returns 'normal, 'code-block-diff, 'code-block-elisp, etc."
  (catch 'context-found
    (save-excursion
      (goto-char pos)
      
      ;; First check if we're ON a code block marker line
      (let ((current-line (buffer-substring-no-properties 
                          (line-beginning-position) 
                          (line-end-position))))
        (when (string-match "^```\\(\\w+\\)?$" (string-trim current-line))
          ;; We're ON the marker line - return normal, not code-block
          (throw 'context-found 'normal)))
      
      ;; Now scan backward to find what block we're inside
      (let ((found-ambiguous-fence nil)
            (ambiguous-fence-pos nil))
        
        ;; Scan backward
        (while (> (point) (point-min))
          (forward-line -1)
          (let* ((line (buffer-substring-no-properties 
                        (line-beginning-position) 
                        (line-end-position)))
                 (trimmed (string-trim line)))
            
            (cond
             ;; Definite Start (e.g. ```python, ```diff) - Has content after backticks
             ((string-match "^```\\(\\w+\\)$" trimmed)
              (if found-ambiguous-fence
                  ;; We found a closing fence (ambiguous) then this Start.
                  ;; The Ambiguous fence closed this block. We are OUTSIDE.
                  (throw 'context-found 'normal)
                ;; We found a Start and no closing fence before it. We are INSIDE.
                (throw 'context-found (list 'code-block (match-string 1 trimmed) (point)))))
             
             ;; Ambiguous/Generic Fence (```) - Empty or just backticks
             ;; Use string-prefix-p to be robust against trailing whitespace
             ((string-prefix-p "```" trimmed)
              (if found-ambiguous-fence
                  ;; We found "```" then "```". 
                  ;; The first one closed the block opened by the second. We are OUTSIDE.
                  (throw 'context-found 'normal)
                
                ;; This is the first fence we've seen scanning backwards.
                ;; It could be an END (if there is a start further back).
                ;; Or it could be a START (if we hit BOF).
                ;; Mark it and keep searching.
                (setq found-ambiguous-fence t)
                (setq ambiguous-fence-pos (point)))))))
        
        ;; We reached the start of the buffer
        (if found-ambiguous-fence
            ;; The fence we found had no start before it. So it IS the start.
            ;; We are INSIDE a generic code block.
            (list 'code-block "generic" ambiguous-fence-pos)
          'normal)))))

(defun aidermacs-eat--collect-code-block-content (start-pos block-type)
  "Collect all lines of a code block starting at START-POS.
Returns (content . line-positions) where line-positions is a list of (start . end) pairs."
  (save-excursion
    (goto-char start-pos)
    (forward-line 1) ; Skip the ```language line
    (let ((content "")
          (line-positions '())
          (continue t))
      (while (and continue (not (eobp)))
        (let ((line-start (line-beginning-position))
              (line-end (line-beginning-position 2)) ; Include newline for :extend face
              (line-content (buffer-substring-no-properties 
                            (line-beginning-position) 
                            (line-end-position))))
          ;; Check for closing fence - any line starting with ``` ends the block
          (if (string-prefix-p "```" (string-trim line-content))
              (setq continue nil)
            (push (cons line-start line-end) line-positions)
            (setq content (concat content line-content "\n"))
            (forward-line 1))))
      (cons content (nreverse line-positions)))))

(defun aidermacs-eat--format-multi-line-code-block (block-start block-type)
  "Format an entire multi-line code block starting at BLOCK-START."
  (let* ((block-data (aidermacs-eat--collect-code-block-content block-start block-type))
         (content (car block-data))
         (line-positions (cdr block-data)))
    
    (when (and content line-positions)
      ;; Step 1: Always apply the base code face (background) to ALL lines first
      (dolist (line-pos line-positions)
        (put-text-property (car line-pos) (cdr line-pos) 'face 'aidermacs-eat-markdown-code-face))
      
      ;; Step 2: Apply syntax highlighting on top (merging faces)
      (let ((properties (aidermacs-eat--get-syntax-highlighting content block-type)))
        (when properties
          (aidermacs-eat--debug-log "Applying multi-line syntax highlighting for %s block (%d lines)" 
                                   block-type (length line-positions))
          (aidermacs-eat--apply-multi-line-syntax-properties content line-positions properties))))))

(defun aidermacs-eat--apply-multi-line-syntax-properties (content line-positions properties)
  "Apply syntax PROPERTIES to multi-line code block.
CONTENT is the full block content, LINE-POSITIONS maps content to buffer positions."
  (let ((content-lines (split-string content "\n"))
        (current-content-pos 0))
    
    (dotimes (line-idx (length line-positions))
      (when (< line-idx (length content-lines))
        (let* ((line-pos (nth line-idx line-positions))
               (line-start (car line-pos))
               (line-end (cdr line-pos))
               (line-content (nth line-idx content-lines))
               (line-content-start current-content-pos)
               (line-content-end (+ current-content-pos (length line-content))))
          
          ;; Apply properties that fall within this line
          (dolist (prop properties)
            (let ((prop-start (nth 0 prop))
                  (prop-end (nth 1 prop))
                  (face (nth 2 prop)))
              
              ;; Check if property overlaps with current line
              (when (and (< prop-start line-content-end)
                         (> prop-end line-content-start))
                (let ((buffer-start (+ line-start (max 0 (- prop-start line-content-start))))
                      (buffer-end (+ line-start (min (length line-content) 
                                                    (- prop-end line-content-start)))))
                  (when (< buffer-start buffer-end)
                    ;; Use add-face-text-property to merge syntax face with base background
                    (add-face-text-property buffer-start buffer-end face))))))
          
          ;; Move to next line in content
          (setq current-content-pos (+ line-content-end 1)))))))

(defun aidermacs-eat--is-inside-search-replace-block-p (pos)
  "Check if POS is inside a SEARCH/REPLACE block by scanning backward."
  (catch 'context-found
    (save-excursion
      (goto-char pos)
      (let ((search-start nil)
            (replace-start nil))
        
        ;; Scan backward to find block markers
        (while (and (> (point) (point-min))
                    (not search-start)
                    (not replace-start))
          (forward-line -1)
          (let ((line (buffer-substring-no-properties 
                      (line-beginning-position) 
                      (line-end-position))))
            (cond
             ;; Found SEARCH start
             ((string-match "^<<<<<<< SEARCH$" (string-trim line))
              (setq search-start (point)))
             
             ;; Found diff separator
             ((string-match "^=======$" (string-trim line))
              (setq replace-start (point)))
             
             ;; Found REPLACE end (we're outside)
             ((string-match "^>>>>>>> REPLACE$" (string-trim line))
              (throw 'context-found 'normal)))))
        
        ;; Return context
        (cond
         (search-start 'search-block)
         (replace-start 'replace-block)
         (t 'normal))))))

;;; Pattern Detection Functions

(defun aidermacs-eat--is-tool-call-line-p (line)
  "Return non-nil if LINE is a tool call block marker."
  (string-match-p "\\[\\w+\\]Tool Call:\\[?/?\\w*\\]?" line))

(defun aidermacs-eat--is-search-marker-p (line)
  "Return non-nil if LINE is a SEARCH marker."
  (string-match-p "^<<<<<<< SEARCH$" (string-trim line)))

(defun aidermacs-eat--is-diff-marker-p (line)
  "Return non-nil if LINE is a diff separator marker."
  (string-match-p "^=======$" (string-trim line)))

(defun aidermacs-eat--is-replace-marker-p (line)
  "Return non-nil if LINE is a REPLACE marker."
  (string-match-p "^>>>>>>> REPLACE$" (string-trim line)))

(defun aidermacs-eat--is-thinking-marker-p (line)
  "Return non-nil if LINE is a THINKING marker."
  (string-match-p "^►[[:space:]]*\\*\\*THINKING\\*\\*" (string-trim line)))

(defun aidermacs-eat--is-answer-marker-p (line)
  "Return non-nil if LINE is an ANSWER marker."
  (string-match-p "^►[[:space:]]*\\*\\*ANSWER\\*\\*" (string-trim line)))

(defun aidermacs-eat--is-context-start-p (line)
  "Return non-nil if LINE is a context block start marker."
  (string-match-p "^<context[[:space:]]+name=\"[^\"]+\">$" (string-trim line)))

(defun aidermacs-eat--is-context-end-p (line)
  "Return non-nil if LINE is a context block end marker."
  (string-match-p "^</context>$" (string-trim line)))

(defun aidermacs-eat--is-interactive-prompt-p (line)
  "Return non-nil if LINE is an interactive prompt."
  (string-match-p "\\?[[:space:]]*(.*\\[[^]]+\\]:$" line))

(defun aidermacs-eat--is-tool-success-p (line)
  "Return non-nil if LINE is a tool success message."
  (string-match-p "^✅" (string-trim line)))

(defun aidermacs-eat--is-tool-error-p (line)
  "Return non-nil if LINE is a tool error message."
  (string-match-p "\\[\\w+\\]ERROR:\\[/?\\w*\\]" line))

(defun aidermacs-eat--is-tool-warning-p (line)
  "Return non-nil if LINE is a tool warning message."
  (string-match-p "\\[\\w+\\]WARNING:\\[/?\\w*\\]" line))

(defun aidermacs-eat--is-markdown-header-p (line)
  "Return non-nil if LINE is a markdown header."
  (let ((trimmed (string-trim line)))
    (and (or (string-match-p "^#+[[:space:]]" trimmed)
             ;; Also match headers with bold markdown like "### **Key Improvements:**"
             (string-match-p "^#+[[:space:]]*\\*\\*.*\\*\\*" trimmed))
         ;; Don't match tool calls or other formatted content
         (not (string-match-p "\\[\\w+\\]" line)))))

(defun aidermacs-eat--get-header-level (content)
  "Get the header level (1-6) from CONTENT, or nil if not a header."
  (when (string-match "^\\(#+\\)[[:space:]]" (string-trim content))
    (length (match-string 1 content))))

(defun aidermacs-eat--get-header-face (level)
  "Get the appropriate face for header LEVEL."
  (pcase level
    (1 'aidermacs-eat-markdown-h1-face)
    (2 'aidermacs-eat-markdown-h2-face)
    (3 'aidermacs-eat-markdown-h3-face)
    (_ 'aidermacs-eat-markdown-header-face))) ; fallback for 4+ or invalid

(defun aidermacs-eat--is-markdown-code-block-p (line)
  "Return non-nil if LINE is a markdown code block marker."
  (string-match-p "^```" (string-trim line)))

(defun aidermacs-eat--is-function-call-p (line)
  "Return non-nil if LINE contains a complete function call like 'module:function()'."
  (and (string-match-p "\\w+:\\w+([^)]*)" line)
       ;; Only match if complete (not cut off)
       (not (string-match-p "\\w+:[^)]*$" line))
       ;; Don't match if it's part of other formatted content
       (not (string-match-p "\\[\\w+\\]" line))))

(defun aidermacs-eat--is-markdown-bold-p (line)
  "Return non-nil if LINE contains complete markdown bold text."
  (and (string-match-p "\\*\\*[^*]+\\*\\*" line)
       ;; Only match if complete (not cut off)
       (not (string-match-p "\\*\\*[^*]*$" line))
       ;; Don't match if it's part of other formatted content
       (not (string-match-p "\\[\\w+\\]" line))
       (not (string-match-p "^#+[[:space:]]" (string-trim line)))))

(defun aidermacs-eat--is-diff-start-p (line)
  "Return non-nil if LINE starts a diff block."
  (string-match-p "^```diff$" (string-trim line)))

;;; Formatting Functions

(defun aidermacs-eat--strip-rich-markup (text)
  "Remove rich text markup like [color]...[/color] from TEXT."
  (replace-regexp-in-string "\\[/?\\w+\\]" "" text))

(defun aidermacs-eat--get-syntax-highlighting (content language)
  "Get syntax highlighting for CONTENT in LANGUAGE.
Returns a list of (start end face) tuples."
  (when-let ((mode (cdr (assoc language aidermacs-eat--language-mode-alist))))
    ;; Check cache first
    (let ((cache-key (cons language (md5 content))))
      (or (gethash cache-key aidermacs-eat--syntax-cache)
          (let ((properties (aidermacs-eat--extract-syntax-properties content mode)))
            ;; Cache the result
            (puthash cache-key properties aidermacs-eat--syntax-cache)
            properties)))))

(defun aidermacs-eat--extract-syntax-properties (content mode)
  "Extract syntax properties from CONTENT using MODE with clean hook isolation."
  (condition-case err
      (let ((temp-buffer (generate-new-buffer " *temp*" t)))
        (with-current-buffer temp-buffer
          (unwind-protect
              (progn
                ;; Use delay-mode-hooks to prevent ALL mode hooks from running
                ;; This prevents flyspell, ispell, and other minor modes from activating
                (delay-mode-hooks
                  (funcall mode)
                  (insert content)
                  (font-lock-ensure))

                ;; Extract face properties for syntax highlighting
                (let ((properties nil)
                      (pos (point-min)))
                  (while (< pos (point-max))
                    (let* ((next-change (next-property-change pos nil (point-max)))
                           (face (get-text-property pos 'face)))
                      (when face
                        (setq properties (cons (list pos next-change face) properties)))
                      (setq pos next-change)))
                  (nreverse properties)))

            ;; Always clean up the temporary buffer
            (when (buffer-name temp-buffer)
              (kill-buffer temp-buffer)))))
    (error
     (message "Error applying syntax highlighting for mode %s: %s" mode err)
     nil)))

(defun aidermacs-eat--apply-syntax-properties (start content properties)
  "Apply PROPERTIES extracted from syntax highlighting to region starting at START."
  (dolist (prop properties)
    (let ((prop-start (+ start (nth 0 prop)))
          (prop-end (+ start (nth 1 prop)))
          (face (nth 2 prop)))
      (when (and (>= prop-start start)
                 (<= prop-end (+ start (length content))))
        (put-text-property prop-start prop-end 'face face)))))

(defun aidermacs-eat--format-code-block-line (start end content block-type)
  "Format a line inside a code block using proper syntax highlighting."
  (cond
   ;; For supported languages, use syntax highlighting
   ((assoc block-type aidermacs-eat--language-mode-alist)
    (let ((properties (aidermacs-eat--get-syntax-highlighting content block-type)))
      (if properties
          (progn
            (aidermacs-eat--debug-log "Applying %d syntax properties for %s" 
                                     (length properties) block-type)
            (aidermacs-eat--apply-syntax-properties start content properties))
        ;; Fallback to basic code face
        (put-text-property start end 'face 'aidermacs-eat-markdown-code-face))))
   
   ;; Special handling for diff blocks
   ((string= block-type "diff")
    (aidermacs-eat--format-diff-line start end content))
   
   ;; Fallback for unsupported languages
   (t
    (put-text-property start end 'face 'aidermacs-eat-markdown-code-face))))

(defun aidermacs-eat--format-diff-line (start end content)
  "Format a diff line with appropriate diff coloring."
  (let ((trimmed (string-trim content)))
    (cond
     ((string-prefix-p "+" trimmed)
      (put-text-property start end 'face 'diff-added))
     ((string-prefix-p "-" trimmed)
      (put-text-property start end 'face 'diff-removed))
     ((string-prefix-p "@@" trimmed)
      (put-text-property start end 'face 'diff-hunk-header))
     ((string-prefix-p "+++" trimmed)
      (put-text-property start end 'face 'diff-file-header))
     ((string-prefix-p "---" trimmed)
      (put-text-property start end 'face 'diff-file-header))
     (t
      (put-text-property start end 'face 'diff-context)))))

(defun aidermacs-eat--format-search-replace-block-line (start end content context)
  "Format a line inside a SEARCH/REPLACE block based on CONTEXT."
  (let ((face (pcase context
                ('search-block 'aidermacs-eat-search-block-face)
                ('replace-block 'aidermacs-eat-replace-block-face)
                (_ nil))))
    (when face
      (put-text-property start end 'face face))))

(defun aidermacs-eat--format-tool-call-with-faces (line)
  "Format a tool call LINE using faces."
  (let ((clean-line (aidermacs-eat--strip-rich-markup line)))
    (aidermacs-eat--apply-face-formatting clean-line 'aidermacs-eat-tool-call-face 'ai-response)))

(defun aidermacs-eat--format-search-block-line-with-faces (line)
  "Format a SEARCH block LINE using diff-removed face."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-search-block-face))

(defun aidermacs-eat--format-replace-block-line-with-faces (line)
  "Format a REPLACE block LINE using diff-added face."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-replace-block-face))

(defun aidermacs-eat--format-thinking-marker-with-faces (line)
  "Format a THINKING marker LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-thinking-face 'ai-response))

(defun aidermacs-eat--format-answer-marker-with-faces (line)
  "Format an ANSWER marker LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-answer-face 'ai-response))

(defun aidermacs-eat--format-context-start-with-faces (line)
  "Format a context block start LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-context-face))

(defun aidermacs-eat--format-context-end-with-faces (line)
  "Format a context block end LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-context-face))

(defun aidermacs-eat--format-tool-success-with-faces (line)
  "Format a tool success LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-success-face))

(defun aidermacs-eat--format-tool-error-with-faces (line)
  "Format a tool error LINE using faces."
  (let ((clean-line (aidermacs-eat--strip-rich-markup line)))
    (aidermacs-eat--apply-face-formatting clean-line 'aidermacs-eat-error-face)))

(defun aidermacs-eat--format-tool-warning-with-faces (line)
  "Format a tool warning LINE using faces."
  (let ((clean-line (aidermacs-eat--strip-rich-markup line)))
    (aidermacs-eat--apply-face-formatting clean-line 'aidermacs-eat-warning-face)))

(defun aidermacs-eat--format-interactive-prompt-with-faces (line)
  "Format an interactive prompt LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-prompt-face 'user-prompt))

(defun aidermacs-eat--format-markdown-header-with-faces (line)
  "Format a markdown header LINE using proper header face based on level."
  (let* ((level (aidermacs-eat--get-header-level line))
         (face (aidermacs-eat--get-header-face level)))
    (aidermacs-eat--apply-face-formatting line face)))

(defun aidermacs-eat--format-markdown-code-block-with-faces (line)
  "Format a markdown code block marker LINE using faces."
  (aidermacs-eat--apply-face-formatting line 'aidermacs-eat-markdown-code-face))

(defun aidermacs-eat--format-function-call-with-faces (line)
  "Format function calls in LINE using faces."
  (let ((result (copy-sequence line)))
    (while (string-match "\\(\\w+:\\w+([^)]*)\\)" result)
      (let ((start (match-beginning 1))
            (end (match-end 1)))
        (put-text-property start end 'face 'aidermacs-eat-function-call-face result)))
    result))

(defun aidermacs-eat--format-markdown-bold-with-faces (line)
  "Format markdown bold text in LINE using faces."
  (let ((result (copy-sequence line)))
    (while (string-match "\\*\\*\\([^*]+\\)\\*\\*" result)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (text (match-string 1 result)))
        ;; Replace **text** with just text
        (setq result (replace-match text nil nil result))
        ;; Apply bold face to the replaced text
        (put-text-property start (+ start (length text)) 'face 
                          'aidermacs-eat-markdown-bold-face result)))
    result))

;;; State Machine

(cl-defstruct (aidermacs-eat--state
               (:constructor aidermacs-eat--make-state)
               (:copier nil))
  "State machine for tracking multi-line blocks."
  (mode 'normal)           ; Current mode: normal, search-block, replace-block, etc.
  (buffer nil)             ; Accumulated text buffer
  (context-depth 0))       ; Nesting depth for context blocks

(defun aidermacs-eat--transition-state (state new-mode)
  "Transition STATE to NEW-MODE."
  (setf (aidermacs-eat--state-mode state) new-mode))

(defun aidermacs-eat--state-append (state text)
  "Append TEXT to STATE buffer."
  (setf (aidermacs-eat--state-buffer state)
        (concat (or (aidermacs-eat--state-buffer state) "") text)))

(defun aidermacs-eat--state-clear-buffer (state)
  "Clear the buffer in STATE."
  (setf (aidermacs-eat--state-buffer state) ""))

(defun aidermacs-eat--state-get-buffer (state)
  "Get the accumulated buffer from STATE."
  (aidermacs-eat--state-buffer state))

;;; Main Filter Function

(defun aidermacs-eat--process-line (state line)
  "Process a single LINE using STATE machine.
Returns the formatted line with text properties (no ANSI)."
  (let ((mode (aidermacs-eat--state-mode state))
        (trimmed (string-trim line)))
    (cond
     ;; Handle SEARCH/REPLACE diff blocks
     ((eq mode 'normal)
      (cond
       ;; SEARCH/REPLACE blocks (highest priority)
       ((aidermacs-eat--is-search-marker-p trimmed)
        (aidermacs-eat--debug-log "Detected SEARCH marker: %s" trimmed)
        (aidermacs-eat--transition-state state 'search-block)
        (aidermacs-eat--format-search-block-line-with-faces line))
       
       ;; Tool calls (high priority)
       ((aidermacs-eat--is-tool-call-line-p trimmed)
        (aidermacs-eat--debug-log "Detected tool call: %s" trimmed)
        (aidermacs-eat--format-tool-call-with-faces line))
       
       ;; AI reasoning blocks
       ((aidermacs-eat--is-thinking-marker-p trimmed)
        (aidermacs-eat--debug-log "Detected thinking marker: %s" trimmed)
        (aidermacs-eat--format-thinking-marker-with-faces line))
       
       ((aidermacs-eat--is-answer-marker-p trimmed)
        (aidermacs-eat--debug-log "Detected answer marker: %s" trimmed)
        (aidermacs-eat--format-answer-marker-with-faces line))
       
       ;; Context blocks
       ((aidermacs-eat--is-context-start-p trimmed)
        (aidermacs-eat--format-context-start-with-faces line))
       
       ((aidermacs-eat--is-context-end-p trimmed)
        (aidermacs-eat--format-context-end-with-faces line))
       
       ;; Tool results
       ((aidermacs-eat--is-tool-success-p trimmed)
        (aidermacs-eat--format-tool-success-with-faces line))
       
       ((aidermacs-eat--is-tool-error-p trimmed)
        (aidermacs-eat--format-tool-error-with-faces line))
       
       ((aidermacs-eat--is-tool-warning-p trimmed)
        (aidermacs-eat--format-tool-warning-with-faces line))
       
       ;; Interactive prompts
       ((aidermacs-eat--is-interactive-prompt-p trimmed)
        (aidermacs-eat--debug-log "Detected interactive prompt: %s" trimmed)
        (aidermacs-eat--format-interactive-prompt-with-faces line))
       
       ;; Function calls (before markdown to avoid conflicts)
       ((aidermacs-eat--is-function-call-p line)
        (aidermacs-eat--format-function-call-with-faces line))
       
       ;; Markdown formatting (lower priority, more specific)
       ((and (aidermacs-eat--is-markdown-header-p line)
             (not (aidermacs-eat--is-tool-call-line-p trimmed)))
        (aidermacs-eat--format-markdown-header-with-faces line))
       
       ((and (aidermacs-eat--is-markdown-bold-p line)
             (not (aidermacs-eat--is-tool-call-line-p trimmed))
             (not (aidermacs-eat--is-markdown-header-p line)))
        (aidermacs-eat--format-markdown-bold-with-faces line))
       
       ((aidermacs-eat--is-markdown-code-block-p trimmed)
        (aidermacs-eat--format-markdown-code-block-with-faces line))
       
       ;; Default: no formatting
       (t line)))

     ((eq mode 'search-block)
      (cond
       ((aidermacs-eat--is-diff-marker-p trimmed)
        (aidermacs-eat--debug-log "Detected diff separator, switching to replace mode")
        (aidermacs-eat--transition-state state 'replace-block)
        (aidermacs-eat--format-search-block-line-with-faces line))
       (t
        (aidermacs-eat--format-search-block-line-with-faces line))))

     ((eq mode 'replace-block)
      (cond
       ((aidermacs-eat--is-replace-marker-p trimmed)
        (aidermacs-eat--debug-log "Detected REPLACE marker, switching to normal mode")
        (aidermacs-eat--transition-state state 'normal)
        (aidermacs-eat--format-replace-block-line-with-faces line))
       (t
        (aidermacs-eat--format-replace-block-line-with-faces line))))

     (t line))))

(defun aidermacs-eat--filter-output (state output)
  "Filter OUTPUT through STATE machine, returning formatted text.
This is the main entry point for the formatter."
  (let ((lines (split-string output "\n" t))
        (result "")
        (result-pos 0))
    (dolist (line lines)
      (let* ((trimmed (string-trim line))
             (formatted (aidermacs-eat--process-line state line))
             (marker-type nil))
        (when formatted
          ;; Determine if this line should be marked
          (cond
           ((or (aidermacs-eat--is-thinking-marker-p trimmed)
                (aidermacs-eat--is-answer-marker-p trimmed)
                (aidermacs-eat--is-tool-call-line-p trimmed))
            (setq marker-type 'ai-response))
           ((aidermacs-eat--is-interactive-prompt-p trimmed)
            (setq marker-type 'user-prompt)))
          
          ;; Add the formatted line to result
          (setq result (concat result formatted "\n"))
          
          ;; Mark the position if needed
          (when marker-type
            (put-text-property result-pos (1+ result-pos) 'aidermacs-marker t result)
            (put-text-property result-pos (1+ result-pos) 'aidermacs-marker-type marker-type result))
          
          ;; Update position for next line
          (setq result-pos (length result)))))
    result))

;;; Face-based Formatting

(defvar-local aidermacs-eat--use-faces nil
  "When non-nil, use Emacs faces instead of ANSI escape sequences.")

(defun aidermacs-eat--apply-face-formatting (text face &optional marker-type)
  "Apply FACE formatting to TEXT with optional MARKER-TYPE.
Returns text with face and marker properties applied."
  (let ((result (copy-sequence text)))
    (put-text-property 0 (length result) 'face face result)
    (when marker-type
      (put-text-property 0 1 'aidermacs-marker t result)
      (put-text-property 0 1 'aidermacs-marker-type marker-type result))
    result))

;;; Debug Function Declaration

(declare-function aidermacs-eat--debug-log "aidermacs-backend-eat")

;;; Public API


;;; Navigation Functions

(defun aidermacs-eat--find-next-marker (&optional type)
  "Find the next marker position from point.
If TYPE is specified, only find markers of that type."
  (let ((pos (point)))
    (cl-block nil
      (while (and pos (< pos (point-max)))
        (setq pos (next-single-property-change pos 'aidermacs-marker))
        (when (and pos
                   (get-text-property pos 'aidermacs-marker)
                   (or (null type)
                       (eq (get-text-property pos 'aidermacs-marker-type) type)))
          (cl-return pos))))))

(defun aidermacs-eat--find-previous-marker (&optional type)
  "Find the previous marker position from point.
If TYPE is specified, only find markers of that type."
  (let ((pos (point)))
    (cl-block nil
      (while (and pos (> pos (point-min)))
        (setq pos (previous-single-property-change pos 'aidermacs-marker))
        (when (and pos
                   (> pos (point-min))
                   (get-text-property pos 'aidermacs-marker)
                   (or (null type)
                       (eq (get-text-property pos 'aidermacs-marker-type) type)))
          (cl-return pos))))))

(defun aidermacs-eat-next-ai-response ()
  "Navigate to the next AI response in the buffer."
  (interactive)
  (let ((pos (aidermacs-eat--find-next-marker 'ai-response)))
    (when pos
      (goto-char pos)
      (when (get-buffer-window)
        (recenter)))))

(defun aidermacs-eat-previous-ai-response ()
  "Navigate to the previous AI response in the buffer."
  (interactive)
  (let ((pos (aidermacs-eat--find-previous-marker 'ai-response)))
    (when pos
      (goto-char pos)
      (when (get-buffer-window)
        (recenter)))))

(defun aidermacs-eat-next-user-prompt ()
  "Navigate to the next user prompt in the buffer."
  (interactive)
  (let ((pos (aidermacs-eat--find-next-marker 'user-prompt)))
    (when pos
      (goto-char pos)
      (when (get-buffer-window)
        (recenter)))))

(defun aidermacs-eat-previous-user-prompt ()
  "Navigate to the previous user prompt in the buffer."
  (interactive)
  (let ((pos (aidermacs-eat--find-previous-marker 'user-prompt)))
    (when pos
      (goto-char pos)
      (when (get-buffer-window)
        (recenter)))))

(defun aidermacs-eat-next-exchange ()
  "Navigate to the next conversation exchange (AI response or user prompt)."
  (interactive)
  (let ((pos (aidermacs-eat--find-next-marker)))
    (when pos
      (goto-char pos)
      (recenter))))

(defun aidermacs-eat-previous-exchange ()
  "Navigate to the previous conversation exchange (AI response or user prompt)."
  (interactive)
  (let ((pos (aidermacs-eat--find-previous-marker)))
    (when pos
      (goto-char pos)
      (recenter))))

(defvar-local aidermacs-eat--formatter-state nil
  "Buffer-local state machine for the Eat formatter.")

(defun aidermacs-eat-setup-formatter ()
  "Set up the Eat formatter for the current buffer.
This should be called when initializing an Eat terminal for aider-ce."
  (unless aidermacs-eat--formatter-state
    (setq-local aidermacs-eat--formatter-state (aidermacs-eat--make-state))))

(defun aidermacs-eat-filter-function (output)
  "Filter function to be attached to Eat process.
Formats OUTPUT using the formatter state machine.
NOTE: This is now used only for reference - actual formatting happens via buffer scanning."
  (unless aidermacs-eat--formatter-state
    (aidermacs-eat-setup-formatter))
  (aidermacs-eat--filter-output aidermacs-eat--formatter-state output))

(provide 'aidermacs-backend-eat-formatter)

;;; aidermacs-backend-eat-formatter.el ends here
