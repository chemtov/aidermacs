;;; aidermacs-backend-eat-formatter-test.el --- Tests for Eat formatter -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Aidermacs Contributors
;; Keywords: tests

;;; Commentary:

;; Comprehensive test suite for the Eat backend formatter that transforms
;; aider-ce's linear output into richly formatted terminal display.

;;; Code:

(require 'ert)
(require 'aidermacs-backend-eat-formatter)

;;; Helper Functions

(defun aidermacs-eat-test--strip-ansi (str)
  "Remove ANSI escape sequences from STR for testing."
  (replace-regexp-in-string "\e\\[[0-9;]*m" "" str))

(defun aidermacs-eat-test--has-ansi-color (str color-code)
  "Check if STR contains ANSI color code COLOR-CODE."
  (string-match-p (regexp-quote (format "\e[%sm" color-code)) str))

;;; Pattern Detection Tests

(ert-deftest aidermacs-eat-test-detect-tool-call-block ()
  "Test detection of tool call blocks."
  (should (aidermacs-eat--is-tool-call-line-p "[blue]Tool Call:[/] web-search • full-web-search"))
  (should (aidermacs-eat--is-tool-call-line-p "[red]Tool Call:[/red] server • tool"))
  (should-not (aidermacs-eat--is-tool-call-line-p "Regular text"))
  (should-not (aidermacs-eat--is-tool-call-line-p "Tool Call without markup")))

(ert-deftest aidermacs-eat-test-detect-search-replace-markers ()
  "Test detection of SEARCH/REPLACE diff markers."
  (should (aidermacs-eat--is-search-marker-p "<<<<<<< SEARCH"))
  (should (aidermacs-eat--is-diff-marker-p "======="))
  (should (aidermacs-eat--is-replace-marker-p ">>>>>>> REPLACE"))
  (should-not (aidermacs-eat--is-search-marker-p "<<< SEARCH"))
  (should-not (aidermacs-eat--is-diff-marker-p "===="))
  (should-not (aidermacs-eat--is-replace-marker-p ">>> REPLACE")))

(ert-deftest aidermacs-eat-test-detect-reasoning-blocks ()
  "Test detection of AI reasoning block markers."
  (should (aidermacs-eat--is-thinking-marker-p "► **THINKING**"))
  (should (aidermacs-eat--is-answer-marker-p "► **ANSWER**"))
  (should-not (aidermacs-eat--is-thinking-marker-p "THINKING"))
  (should-not (aidermacs-eat--is-answer-marker-p "ANSWER")))

(ert-deftest aidermacs-eat-test-detect-context-blocks ()
  "Test detection of context block markers (agent mode)."
  (should (aidermacs-eat--is-context-start-p "<context name=\"git_status\">"))
  (should (aidermacs-eat--is-context-start-p "<context name=\"context_summary\">"))
  (should (aidermacs-eat--is-context-end-p "</context>"))
  (should-not (aidermacs-eat--is-context-start-p "<context>"))
  (should-not (aidermacs-eat--is-context-end-p "</context")))

(ert-deftest aidermacs-eat-test-detect-interactive-prompts ()
  "Test detection of interactive prompts."
  (should (aidermacs-eat--is-interactive-prompt-p "Run tools? (Y)es/(N)o/(A)ll/(S)kip all [Yes]:"))
  (should (aidermacs-eat--is-interactive-prompt-p "Edit the files? (Y)es/(N)o/(T)weak [Yes]:"))
  (should-not (aidermacs-eat--is-interactive-prompt-p "This is a question?")))

(ert-deftest aidermacs-eat-test-detect-tool-results ()
  "Test detection of tool result messages."
  (should (aidermacs-eat--is-tool-success-p "✅ Replaced 5 lines in path/to/file.py (change_id: 1a2b3c-4d5e)"))
  (should (aidermacs-eat--is-tool-error-p "[red]ERROR:[/] Error in ReplaceText: File not found"))
  (should (aidermacs-eat--is-tool-warning-p "[yellow]WARNING:[/] Something might be wrong"))
  (should-not (aidermacs-eat--is-tool-success-p "Success without checkmark"))
  (should-not (aidermacs-eat--is-tool-error-p "ERROR without markup")))

;;; Formatting Tests

(ert-deftest aidermacs-eat-test-format-tool-call ()
  "Test formatting of tool call blocks."
  (let* ((input "[blue]Tool Call:[/] web-search • full-web-search")
         (output (aidermacs-eat--format-tool-call input)))
    (should (stringp output))
    (should (aidermacs-eat-test--has-ansi-color output "34")) ; Blue
    (should (string-match-p "Tool Call:" output))))

(ert-deftest aidermacs-eat-test-format-search-block ()
  "Test formatting of SEARCH block with red background."
  (let* ((input "old code to be replaced")
         (output (aidermacs-eat--format-search-block-line input)))
    (should (stringp output))
    (should (aidermacs-eat-test--has-ansi-color output "41")) ; Red background
    (should (string-match-p "old code" output))))

(ert-deftest aidermacs-eat-test-format-replace-block ()
  "Test formatting of REPLACE block with green background."
  (let* ((input "new code to replace with")
         (output (aidermacs-eat--format-replace-block-line input)))
    (should (stringp output))
    (should (aidermacs-eat-test--has-ansi-color output "42")) ; Green background
    (should (string-match-p "new code" output))))

(ert-deftest aidermacs-eat-test-format-thinking-block ()
  "Test formatting of THINKING block."
  (let* ((input "► **THINKING**")
         (output (aidermacs-eat--format-thinking-marker input)))
    (should (stringp output))
    (should (aidermacs-eat-test--has-ansi-color output "33")) ; Yellow
    (should (string-match-p "THINKING" output))))

(ert-deftest aidermacs-eat-test-format-context-block ()
  "Test formatting of context block headers."
  (let* ((input "<context name=\"git_status\">")
         (output (aidermacs-eat--format-context-start input)))
    (should (stringp output))
    (should (aidermacs-eat-test--has-ansi-color output "36")) ; Cyan
    (should (string-match-p "git_status" output))))

;;; State Machine Tests

(ert-deftest aidermacs-eat-test-state-machine-init ()
  "Test state machine initialization."
  (let ((state (aidermacs-eat--make-state)))
    (should (eq (aidermacs-eat--state-mode state) 'normal))
    (should (null (aidermacs-eat--state-buffer state)))))

(ert-deftest aidermacs-eat-test-state-transition-to-search ()
  "Test state transition from normal to search block."
  (let ((state (aidermacs-eat--make-state)))
    (aidermacs-eat--transition-state state 'search-block)
    (should (eq (aidermacs-eat--state-mode state) 'search-block))))

(ert-deftest aidermacs-eat-test-state-transition-to-replace ()
  "Test state transition from search to replace block."
  (let ((state (aidermacs-eat--make-state)))
    (aidermacs-eat--transition-state state 'search-block)
    (aidermacs-eat--transition-state state 'replace-block)
    (should (eq (aidermacs-eat--state-mode state) 'replace-block))))

(ert-deftest aidermacs-eat-test-state-buffer-accumulation ()
  "Test accumulation of lines in state buffer."
  (let ((state (aidermacs-eat--make-state)))
    (aidermacs-eat--state-append state "line 1\n")
    (aidermacs-eat--state-append state "line 2\n")
    (should (equal (aidermacs-eat--state-buffer state) "line 1\nline 2\n"))))

;;; Filter Function Tests

(ert-deftest aidermacs-eat-test-filter-passthrough ()
  "Test that normal text passes through unchanged."
  (let* ((state (aidermacs-eat--make-state))
         (input "Regular text without special formatting\n")
         (output (aidermacs-eat--filter-output state input)))
    (should (equal (aidermacs-eat-test--strip-ansi output) input))))

(ert-deftest aidermacs-eat-test-filter-tool-call ()
  "Test filtering of tool call block."
  (let* ((state (aidermacs-eat--make-state))
         (input "[blue]Tool Call:[/] server • tool\n")
         (output (aidermacs-eat--filter-output state input)))
    (should (stringp output))
    (should (aidermacs-eat-test--has-ansi-color output "34"))))

(ert-deftest aidermacs-eat-test-filter-complete-diff-block ()
  "Test filtering of complete SEARCH/REPLACE diff block."
  (let* ((state (aidermacs-eat--make-state))
         (input "<<<<<<< SEARCH
old code
=======
new code
>>>>>>> REPLACE
")
         (output (aidermacs-eat--filter-output state input)))
    (should (stringp output))
    ;; Should contain red background for search
    (should (string-match-p "\e\\[41m" output))
    ;; Should contain green background for replace
    (should (string-match-p "\e\\[42m" output))))

;;; Functional Simulation Tests


;;; Navigation Tests

(ert-deftest aidermacs-eat-test-mark-ai-response ()
  "Test marking AI response positions."
  (with-temp-buffer
    (insert "Some text\n")
    (let ((pos (point)))
      (insert "► **THINKING**\n")
      (aidermacs-eat--mark-position pos 'ai-response)
      (should (get-text-property pos 'aidermacs-marker))
      (should (eq (get-text-property pos 'aidermacs-marker-type) 'ai-response)))))

(ert-deftest aidermacs-eat-test-mark-user-prompt ()
  "Test marking user prompt positions."
  (with-temp-buffer
    (insert "Some text\n")
    (let ((pos (point)))
      (insert "Run tools? (Y)es/(N)o [Yes]:\n")
      (aidermacs-eat--mark-position pos 'user-prompt)
      (should (get-text-property pos 'aidermacs-marker))
      (should (eq (get-text-property pos 'aidermacs-marker-type) 'user-prompt)))))

(ert-deftest aidermacs-eat-test-find-next-marker ()
  "Test finding next marker from current position."
  (with-temp-buffer
    (insert "Text before\n")
    (let ((marker1 (point)))
      (insert "► **THINKING**\n")
      (aidermacs-eat--mark-position marker1 'ai-response)
      (insert "More text\n")
      (let ((marker2 (point)))
        (insert "Run tools? (Y)es/(N)o [Yes]:\n")
        (aidermacs-eat--mark-position marker2 'user-prompt)
        (goto-char (point-min))
        ;; Find first marker
        (let ((next (aidermacs-eat--find-next-marker)))
          (should (= next marker1)))
        ;; Find second marker
        (goto-char (1+ marker1))
        (let ((next (aidermacs-eat--find-next-marker)))
          (should (= next marker2)))))))

(ert-deftest aidermacs-eat-test-find-previous-marker ()
  "Test finding previous marker from current position."
  (with-temp-buffer
    (insert "Text before\n")
    (let ((marker1 (point)))
      (insert "► **THINKING**\n")
      (aidermacs-eat--mark-position marker1 'ai-response)
      (insert "More text\n")
      (let ((marker2 (point)))
        (insert "Run tools? (Y)es/(N)o [Yes]:\n")
        (aidermacs-eat--mark-position marker2 'user-prompt)
        (goto-char (point-max))
        ;; Find second marker
        (let ((prev (aidermacs-eat--find-previous-marker)))
          (should (= prev marker2)))
        ;; Find first marker
        (goto-char (1- marker2))
        (let ((prev (aidermacs-eat--find-previous-marker)))
          (should (= prev marker1)))))))

(ert-deftest aidermacs-eat-test-navigate-to-next-ai-response ()
  "Test navigation to next AI response."
  (with-temp-buffer
    (insert "User input\n")
    (let ((ai-pos (point)))
      (insert "► **THINKING**\nAI reasoning\n")
      (aidermacs-eat--mark-position ai-pos 'ai-response)
      (goto-char (point-min))
      (aidermacs-eat-next-ai-response)
      (should (= (point) ai-pos)))))

(ert-deftest aidermacs-eat-test-navigate-to-previous-ai-response ()
  "Test navigation to previous AI response."
  (with-temp-buffer
    (insert "Text before\n")
    (let ((ai-pos (point)))
      (insert "► **THINKING**\nAI reasoning\n")
      (aidermacs-eat--mark-position ai-pos 'ai-response)
      (insert "More text\n")
      (goto-char (point-max))
      (aidermacs-eat-previous-ai-response)
      (should (= (point) ai-pos)))))

(ert-deftest aidermacs-eat-test-navigate-to-next-user-prompt ()
  "Test navigation to next user prompt."
  (with-temp-buffer
    (insert "AI response\n")
    (let ((prompt-pos (point)))
      (insert "Run tools? (Y)es/(N)o [Yes]:\n")
      (aidermacs-eat--mark-position prompt-pos 'user-prompt)
      (goto-char (point-min))
      (aidermacs-eat-next-user-prompt)
      (should (= (point) prompt-pos)))))

(ert-deftest aidermacs-eat-test-navigate-to-previous-user-prompt ()
  "Test navigation to previous user prompt."
  (with-temp-buffer
    (insert "Text before\n")
    (let ((prompt-pos (point)))
      (insert "Run tools? (Y)es/(N)o [Yes]:\n")
      (aidermacs-eat--mark-position prompt-pos 'user-prompt)
      (insert "More text\n")
      (goto-char (point-max))
      (aidermacs-eat-previous-user-prompt)
      (should (= (point) prompt-pos)))))

(ert-deftest aidermacs-eat-test-filter-with-markers ()
  "Test that filtering adds markers to output."
  (with-temp-buffer
    (let ((state (aidermacs-eat--make-state)))
      (aidermacs-eat-setup-formatter)
      (let ((output (aidermacs-eat--filter-output state "► **THINKING**\nReasoning here\n")))
        ;; Check that the output string has text properties
        (should (stringp output))
        ;; Find the first position with the marker property
        (let ((pos 0)
              (found nil))
          (while (and (< pos (length output)) (not found))
            (when (get-text-property pos 'aidermacs-marker output)
              (setq found pos))
            (setq pos (1+ pos)))
          (should found)
          (should (eq (get-text-property found 'aidermacs-marker-type output) 'ai-response)))))))

(ert-deftest aidermacs-eat-test-simulate-agent-session ()
  "Functional test simulating a complete agent mode session."
  (let* ((state (aidermacs-eat--make-state))
         (transcript "<context name=\"git_status\">
## Git Repository Status
Current branch: main
</context>

[blue]Tool Call:[/] git • status
[blue]Arguments:[/] {}

✅ Command executed successfully

► **THINKING**
The user wants to check git status.
------------
► **ANSWER**
The repository is on the main branch.
")
         (output (aidermacs-eat--filter-output state transcript)))
    (should (stringp output))
    ;; Should have context block formatting (cyan)
    (should (string-match-p "\e\\[36m" output))
    ;; Should have tool call formatting (blue)
    (should (string-match-p "\e\\[34m" output))
    ;; Should have success marker
    (should (string-match-p "✅" output))
    ;; Should have thinking block formatting (yellow)
    (should (string-match-p "\e\\[33m" output))))

(ert-deftest aidermacs-eat-test-simulate-code-edit ()
  "Functional test simulating a code editing session."
  (let* ((state (aidermacs-eat--make-state))
         (transcript "file.py
<<<<<<< SEARCH
def old_function():
    pass
=======
def new_function():
    return True
>>>>>>> REPLACE

✅ Replaced 2 lines in file.py (change_id: abc123)
")
         (output (aidermacs-eat--filter-output state transcript)))
    (should (stringp output))
    ;; Should have red background for search block
    (should (string-match-p "\e\\[41m" output))
    ;; Should have green background for replace block
    (should (string-match-p "\e\\[42m" output))
    ;; Should have success marker
    (should (string-match-p "✅" output))))

(ert-deftest aidermacs-eat-test-multiline-buffering ()
  "Test that multi-line blocks are buffered correctly."
  (let ((state (aidermacs-eat--make-state)))
    ;; Send incomplete diff block in chunks
    (aidermacs-eat--filter-output state "<<<<<<< SEARCH\n")
    (should (eq (aidermacs-eat--state-mode state) 'search-block))

    (aidermacs-eat--filter-output state "old code\n")
    (should (eq (aidermacs-eat--state-mode state) 'search-block))

    (aidermacs-eat--filter-output state "=======\n")
    (should (eq (aidermacs-eat--state-mode state) 'replace-block))

    (let ((output (aidermacs-eat--filter-output state "new code\n>>>>>>> REPLACE\n")))
      (should (eq (aidermacs-eat--state-mode state) 'normal))
      (should (stringp output)))))

(provide 'aidermacs-backend-eat-formatter-test)

;;; aidermacs-backend-eat-formatter-test.el ends here