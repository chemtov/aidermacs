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

;;; ANSI Color Codes

(defconst aidermacs-eat--ansi-reset "\e[0m"
  "ANSI escape sequence to reset all formatting.")

(defconst aidermacs-eat--ansi-bold "\e[1m"
  "ANSI escape sequence for bold text.")

(defconst aidermacs-eat--ansi-colors
  '((black . "30")
    (red . "31")
    (green . "32")
    (yellow . "33")
    (blue . "34")
    (magenta . "35")
    (cyan . "36")
    (white . "37"))
  "Alist mapping color names to ANSI foreground color codes.")

(defconst aidermacs-eat--ansi-bg-colors
  '((black . "40")
    (red . "41")
    (green . "42")
    (yellow . "43")
    (blue . "44")
    (magenta . "45")
    (cyan . "46")
    (white . "47"))
  "Alist mapping color names to ANSI background color codes.")

;;; Helper Functions

(defun aidermacs-eat--ansi-fg (color)
  "Return ANSI escape sequence for foreground COLOR."
  (format "\e[%sm" (alist-get color aidermacs-eat--ansi-colors "37")))

(defun aidermacs-eat--ansi-bg (color)
  "Return ANSI escape sequence for background COLOR."
  (format "\e[%sm" (alist-get color aidermacs-eat--ansi-bg-colors "40")))

(defun aidermacs-eat--colorize (text color &optional bg-color bold)
  "Colorize TEXT with foreground COLOR and optional BG-COLOR.
If BOLD is non-nil, make text bold."
  (concat
   (when bold aidermacs-eat--ansi-bold)
   (aidermacs-eat--ansi-fg color)
   (when bg-color (aidermacs-eat--ansi-bg bg-color))
   text
   aidermacs-eat--ansi-reset))

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

;;; Formatting Functions

(defun aidermacs-eat--strip-rich-markup (text)
  "Remove rich text markup like [color]...[/color] from TEXT."
  (replace-regexp-in-string "\\[/?\\w+\\]" "" text))

(defun aidermacs-eat--format-tool-call (line)
  "Format a tool call LINE with blue color."
  (let ((clean-line (aidermacs-eat--strip-rich-markup line)))
    (aidermacs-eat--colorize clean-line 'blue nil t)))

(defun aidermacs-eat--format-search-block-line (line)
  "Format a SEARCH block LINE with red background."
  (aidermacs-eat--colorize line 'white 'red))

(defun aidermacs-eat--format-replace-block-line (line)
  "Format a REPLACE block LINE with green background."
  (aidermacs-eat--colorize line 'white 'green))

(defun aidermacs-eat--format-thinking-marker (line)
  "Format a THINKING marker LINE with yellow color."
  (aidermacs-eat--colorize line 'yellow nil t))

(defun aidermacs-eat--format-answer-marker (line)
  "Format an ANSWER marker LINE with green color."
  (aidermacs-eat--colorize line 'green nil t))

(defun aidermacs-eat--format-context-start (line)
  "Format a context block start LINE with cyan color."
  (aidermacs-eat--colorize line 'cyan nil t))

(defun aidermacs-eat--format-context-end (line)
  "Format a context block end LINE with cyan color."
  (aidermacs-eat--colorize line 'cyan nil t))

(defun aidermacs-eat--format-tool-success (line)
  "Format a tool success LINE with green color."
  (aidermacs-eat--colorize line 'green nil t))

(defun aidermacs-eat--format-tool-error (line)
  "Format a tool error LINE with red color."
  (let ((clean-line (aidermacs-eat--strip-rich-markup line)))
    (aidermacs-eat--colorize clean-line 'red nil t)))

(defun aidermacs-eat--format-tool-warning (line)
  "Format a tool warning LINE with yellow color."
  (let ((clean-line (aidermacs-eat--strip-rich-markup line)))
    (aidermacs-eat--colorize clean-line 'yellow nil t)))

(defun aidermacs-eat--format-interactive-prompt (line)
  "Format an interactive prompt LINE with magenta color."
  (aidermacs-eat--colorize line 'magenta nil t))

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
Returns the formatted line or nil if buffering."
  (let ((mode (aidermacs-eat--state-mode state))
        (trimmed (string-trim line)))
    (cond
     ;; Handle SEARCH/REPLACE diff blocks
     ((eq mode 'normal)
      (cond
       ((aidermacs-eat--is-search-marker-p trimmed)
        (aidermacs-eat--transition-state state 'search-block)
        (aidermacs-eat--format-search-block-line line))
       ((aidermacs-eat--is-tool-call-line-p trimmed)
        (aidermacs-eat--format-tool-call line))
       ((aidermacs-eat--is-thinking-marker-p trimmed)
        (aidermacs-eat--format-thinking-marker line))
       ((aidermacs-eat--is-answer-marker-p trimmed)
        (aidermacs-eat--format-answer-marker line))
       ((aidermacs-eat--is-context-start-p trimmed)
        (aidermacs-eat--format-context-start line))
       ((aidermacs-eat--is-context-end-p trimmed)
        (aidermacs-eat--format-context-end line))
       ((aidermacs-eat--is-tool-success-p trimmed)
        (aidermacs-eat--format-tool-success line))
       ((aidermacs-eat--is-tool-error-p trimmed)
        (aidermacs-eat--format-tool-error line))
       ((aidermacs-eat--is-tool-warning-p trimmed)
        (aidermacs-eat--format-tool-warning line))
       ((aidermacs-eat--is-interactive-prompt-p trimmed)
        (aidermacs-eat--format-interactive-prompt line))
       (t line)))

     ((eq mode 'search-block)
      (cond
       ((aidermacs-eat--is-diff-marker-p trimmed)
        (aidermacs-eat--transition-state state 'replace-block)
        (aidermacs-eat--format-search-block-line line))
       (t
        (aidermacs-eat--format-search-block-line line))))

     ((eq mode 'replace-block)
      (cond
       ((aidermacs-eat--is-replace-marker-p trimmed)
        (aidermacs-eat--transition-state state 'normal)
        (aidermacs-eat--format-replace-block-line line))
       (t
        (aidermacs-eat--format-replace-block-line line))))

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

;;; Public API


;;; Navigation Functions

(defun aidermacs-eat--mark-position (pos type)
  "Mark position POS with marker TYPE.
TYPE can be 'ai-response or 'user-prompt."
  (put-text-property pos (1+ pos) 'aidermacs-marker t)
  (put-text-property pos (1+ pos) 'aidermacs-marker-type type))

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
Formats OUTPUT using the formatter state machine."
  (unless aidermacs-eat--formatter-state
    (aidermacs-eat-setup-formatter))
  (aidermacs-eat--filter-output aidermacs-eat--formatter-state output))

(provide 'aidermacs-backend-eat-formatter)

;;; aidermacs-backend-eat-formatter.el ends here