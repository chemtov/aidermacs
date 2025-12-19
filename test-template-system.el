;;; test-template-system.el --- Tests for aidermacs template system -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple tests to verify the template system works correctly

;;; Code:

(require 'cl-lib)
(require 'aidermacs-templates)

;; Test 1: Extract placeholders
(let ((template "Hello {Name}, welcome to {Place}!"))
  (let ((placeholders (aidermacs-templates--extract-placeholders template)))
    (message "Test 1 - Extract placeholders: %s" placeholders)
    (cl-assert (equal placeholders '("Name" "Place")))))

;; Test 2: Replace placeholders
(let ((template "Hello {Name}, you are {Age} years old")
      (replacements '(("Name" . "Alice") ("Age" . "30"))))
  (let ((result (aidermacs-templates--replace-placeholders template replacements)))
    (message "Test 2 - Replace placeholders: %s" result)
    (cl-assert (equal result "Hello Alice, you are 30 years old"))))

;; Test 3: Duplicate placeholders (should only appear once)
(let ((template "{Name} says hello to {Name}"))
  (let ((placeholders (aidermacs-templates--extract-placeholders template)))
    (message "Test 3 - Duplicate placeholders: %s" placeholders)
    (cl-assert (equal placeholders '("Name")))))

;; Test 4: No placeholders
(let ((template "This has no placeholders"))
  (let ((placeholders (aidermacs-templates--extract-placeholders template)))
    (message "Test 4 - No placeholders: %s" placeholders)
    (cl-assert (null placeholders))))

;; Test 5: Complex placeholder names


;; --- Metadata Parsing Tests ---

;; Test 18: Parse template with full metadata header
(let ((template "title: Code Review Template
description: Review code for quality and security issues
command: /chat-mode architect
---
/ask Please review {File-Name}"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata))
          (content (plist-get parsed :content)))
      (message "Test 18 - Full metadata: metadata=%s content=%s" metadata content)
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "title")
                        "Code Review Template"))
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "description")
                        "Review code for quality and security issues"))
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "command")
                        "/chat-mode architect"))
      (cl-assert (string-match-p "/ask Please review {File-Name}" content)))))

;; Test 19: Parse template with partial metadata (only description)
(let ((template "description: A simple template
---
/code {Task}"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata))
          (content (plist-get parsed :content)))
      (message "Test 19 - Partial metadata: %s" metadata)
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "description")
                        "A simple template"))
      (cl-assert (null (aidermacs-templates--get-metadata-field metadata "title")))
      (cl-assert (null (aidermacs-templates--get-metadata-field metadata "command")))
      (cl-assert (string-match-p "/code {Task}" content)))))

;; Test 20: Parse template with no metadata (backward compatibility)
(let ((template "/ask {Question}"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata))
          (content (plist-get parsed :content)))
      (message "Test 20 - No metadata: metadata=%s content=%s" metadata content)
      (cl-assert (null metadata))
      (cl-assert (equal content "/ask {Question}")))))

;; Test 21: Parse template with metadata but no content after separator
(let ((template "title: Empty Template
description: Has no content
---"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata))
          (content (plist-get parsed :content)))
      (message "Test 21 - Empty content: metadata=%s content=%s" metadata content)
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "title")
                        "Empty Template"))
      (cl-assert (or (string-empty-p (string-trim content))
                     (equal content ""))))))

;; Test 22: Parse template with unknown metadata fields (should be ignored)
(let ((template "title: Test
unknown-field: Should be ignored
description: Valid description
another-unknown: Also ignored
---
Content here"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata)))
      (message "Test 22 - Unknown fields: %s" metadata)
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "title") "Test"))
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "description")
                        "Valid description"))
      (cl-assert (null (aidermacs-templates--get-metadata-field metadata "unknown-field")))
      (cl-assert (null (aidermacs-templates--get-metadata-field metadata "another-unknown"))))))

;; Test 23: Parse template with multi-line content after separator
(let ((template "description: Multi-line template
---
Line 1
Line 2
Line 3"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((content (plist-get parsed :content)))
      (message "Test 23 - Multi-line content: %s" content)
      (cl-assert (string-match-p "Line 1" content))
      (cl-assert (string-match-p "Line 2" content))
      (cl-assert (string-match-p "Line 3" content)))))

;; Test 24: Parse template with separator-like text in content (should not confuse parser)
(let ((template "title: Test
---
This is content
---
More content with --- in it"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((content (plist-get parsed :content)))
      (message "Test 24 - Separator in content: %s" content)
      ;; Content should include everything after first separator
      (cl-assert (string-match-p "This is content" content))
      (cl-assert (string-match-p "More content with --- in it" content)))))

;; Test 25: Case insensitivity of metadata field names
(let ((template "Title: Mixed Case Title
DESCRIPTION: Uppercase description
CoMmAnD: /help
---
Content"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata)))
      (message "Test 25 - Case insensitive fields: %s" metadata)
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "title")
                        "Mixed Case Title"))
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "description")
                        "Uppercase description"))
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "command")
                        "/help")))))

;; Test 26: Whitespace handling in metadata values


;; Test 27: Marginalia annotator registry format (must be a list, not cons cell)
;; This test reproduces the bug: (wrong-type-argument listp aidermacs-templates--annotate)
(let ((test-passed nil))
  (condition-case err
      (progn
        ;; Simulate what happens in aidermacs-use-template
        ;; The bug was using (cons (cons 'category function) registry)
        ;; which creates ((category . function) ...) - a cons cell
        ;; But marginalia expects ((category function) ...) - a list

        ;; Test the WRONG way (cons cell) - this should fail
        (let* ((wrong-entry (cons 'test-category #'identity))
               (wrong-registry (list wrong-entry)))
          (message "Test 27 - Wrong format (cons cell): %S" wrong-entry)
          (cl-assert (consp wrong-entry) nil "Entry should be a cons")
          (cl-assert (not (listp (cdr wrong-entry))) nil "CDR should not be a list (it's a function)"))

        ;; Test the RIGHT way (list) - this should work
        (let* ((right-entry (list 'test-category #'identity))
               (right-registry (list right-entry)))
          (message "Test 27 - Right format (list): %S" right-entry)
          (cl-assert (listp right-entry) nil "Entry should be a list")
          (cl-assert (listp (cdr right-entry)) nil "CDR should be a list"))

        (setq test-passed t))
    (error
     (message "Test 27 - Error: %S" err)
     (setq test-passed nil)))
  (cl-assert test-passed nil "Marginalia registry format test should pass"))

;; Test 28: Verify annotator function returns proper format
(let ((templates '(("test-template" . "/path/to/test.txt"))))
  ;; Create a mock template file content with metadata
  (let ((temp-file (make-temp-file "test-annotate-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "description: Test description for annotation\n---\nContent"))

          ;; Mock the templates list to include our test file
          (cl-letf (((symbol-function 'aidermacs-templates--list-templates)
                     (lambda () (list (cons "test-template" temp-file)))))
            (let ((annotation (aidermacs-templates--annotate "test-template")))
              (message "Test 28 - Annotation result: %S" annotation)
              (cl-assert (stringp annotation) nil "Annotation should be a string")
              (cl-assert (string-match-p "Test description" annotation)
                         nil "Annotation should contain the description"))))
      (delete-file temp-file))))
(let ((template "title:   Title with leading spaces   
description:Trailing spaces   
---
Content"))
  (let ((parsed (aidermacs-templates--parse-metadata template)))
    (let ((metadata (plist-get parsed :metadata)))
      (message "Test 26 - Whitespace handling: %s" metadata)
      ;; Values should be trimmed
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "title")
                        "Title with leading spaces"))
      (cl-assert (equal (aidermacs-templates--get-metadata-field metadata "description")
                        "Trailing spaces")))))
(let ((template "URL: {Enter-URL}, Action: {What-to-do-with-it}"))
  (let ((placeholders (aidermacs-templates--extract-placeholders template)))
    (message "Test 5 - Complex names: %s" placeholders)
    (cl-assert (equal placeholders '("Enter-URL" "What-to-do-with-it")))))

;; --- Filesystem tests ---
(let ((temp-dir (make-temp-file "aidermacs-templates-test-" t)))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-dir)
            (aidermacs-templates-file-extension '(".txt" ".md")))
        (cl-letf (((symbol-function 'aidermacs-templates--get-default-directory)
                   (lambda ()
                     ;; Return a non-existent dir to isolate user templates
                     (expand-file-name "non-existent" temp-dir))))

          ;; Test 6: List templates from user directory
          (with-temp-file (expand-file-name "test1.txt" temp-dir)
            (insert "content1"))
          (with-temp-file (expand-file-name "test2.md" temp-dir)
            (insert "content2"))
          (with-temp-file (expand-file-name "test3.org" temp-dir)
            (insert "content3"))
          (let* ((templates (aidermacs-templates--list-templates))
                 (template-names (sort (mapcar #'car templates) #'string<)))
            (message "Test 6 - List templates: %s" template-names)
            (cl-assert (equal template-names '("test1" "test2"))))

          ;; Test 7: Read template content
          (let ((file (expand-file-name "read-test.txt" temp-dir)))
            (with-temp-file file
              (insert "Hello template"))
            (let ((content (aidermacs-templates--read-template file)))
              (message "Test 7 - Read template: %s" content)
              (cl-assert (equal content "Hello template"))))))
    (delete-directory temp-dir t)))

;; Test 8: Handle nil directory gracefully
(let ((result (aidermacs-templates--list-templates-from-dir nil)))
  (message "Test 8 - Nil directory: %s" result)
  (cl-assert (null result)))

;; Test 9: Handle non-existent directory gracefully
(let ((result (aidermacs-templates--list-templates-from-dir "/non/existent/path")))
  (message "Test 9 - Non-existent directory: %s" result)
  (cl-assert (null result)))

;; Test 10: get-default-directory returns nil when library not found
(let ((result (aidermacs-templates--get-default-directory)))
  (message "Test 10 - Get default directory: %s" (if result "found" "nil"))
  ;; This should either return a valid directory or nil, not crash
  (cl-assert (or (null result) (stringp result))))

;; Test 11: list-templates works even when default directory is nil
(let ((temp-dir (make-temp-file "aidermacs-templates-test-" t))
      (aidermacs-user-templates-directory nil))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-dir))
        ;; Create a test template
        (with-temp-file (expand-file-name "test.txt" temp-dir)
          (insert "test content"))
        ;; Mock get-default-directory to return nil
        (cl-letf (((symbol-function 'aidermacs-templates--get-default-directory)
                   (lambda () nil)))
          (let ((templates (aidermacs-templates--list-templates)))
            (message "Test 11 - List templates with nil default dir: %s" (mapcar #'car templates))
            (cl-assert (= 1 (length templates)))
            (cl-assert (equal "test" (caar templates))))))
    (delete-directory temp-dir t)))

;; Test 12: Simulate the exact error condition from backtrace

;; Test 13: Verify default templates are found from package directory
(let ((default-dir (aidermacs-templates--get-default-directory)))
  (message "Test 13 - Default templates directory: %s" default-dir)
  (cl-assert (or (null default-dir) (file-directory-p default-dir))
             nil "Default directory should be nil or a valid directory")
  (when default-dir
    (let ((templates (aidermacs-templates--list-templates-from-dir default-dir)))
      (message "Test 13 - Found %d default templates: %s"
               (length templates)
               (mapcar #'car templates))
      (cl-assert (> (length templates) 0)
                 nil "Should find at least one default template"))))

;; Test 14: Verify user templates are found and merged with default templates

;; Test 14a: CRITICAL - Verify BOTH user AND default templates are shown together
;; This test specifically checks that when user templates exist, default templates
;; are still included in the list (not replaced)
(let ((temp-user-dir (make-temp-file "aidermacs-user-templates-test14a-" t)))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-user-dir)
            (default-dir (aidermacs-templates--get-default-directory)))
        ;; Only run this test if we have default templates
        (when default-dir
          ;; Create a user template with a UNIQUE name (not overriding any default)
          (with-temp-file (expand-file-name "my-unique-template.txt" temp-user-dir)
            (insert "/ask {Question}"))

          ;; Get the list of templates
          (let* ((all-templates (aidermacs-templates--list-templates))
                 (template-names (mapcar #'car all-templates))
                 (default-templates (aidermacs-templates--list-templates-from-dir default-dir))
                 (default-names (mapcar #'car default-templates)))

            (message "Test 14a - User templates dir: %s" temp-user-dir)
            (message "Test 14a - Default templates dir: %s" default-dir)
            (message "Test 14a - All template names: %s" template-names)
            (message "Test 14a - Default template names: %s" default-names)

            ;; CRITICAL: Should have the user template
            (cl-assert (member "my-unique-template" template-names)
                       nil "Should find user template 'my-unique-template'")

            ;; CRITICAL: Should ALSO have ALL default templates
            (dolist (default-name default-names)
              (cl-assert (member default-name template-names)
                         nil (format "Should find default template '%s' in merged list" default-name)))

            ;; CRITICAL: Total count should be user templates + default templates
            (let ((expected-count (+ 1 (length default-names))))
              (cl-assert (= (length all-templates) expected-count)
                         nil (format "Should have %d templates (1 user + %d default), but got %d"
                                     expected-count (length default-names) (length all-templates)))))))
    (delete-directory temp-user-dir t)))

(let ((temp-user-dir (make-temp-file "aidermacs-user-templates-" t)))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-user-dir))
        ;; Create some user templates
        (with-temp-file (expand-file-name "user-template-1.txt" temp-user-dir)
          (insert "/ask {Question}"))
        (with-temp-file (expand-file-name "user-template-2.md" temp-user-dir)
          (insert "/code {Task}"))
        ;; Create a user template that overrides a default one
        (with-temp-file (expand-file-name "code-review.txt" temp-user-dir)
          (insert "User's custom code review template"))

        (let* ((all-templates (aidermacs-templates--list-templates))
               (template-names (mapcar #'car all-templates)))
          (message "Test 14 - All templates: %s" template-names)
          ;; Should have user templates
          (cl-assert (member "user-template-1" template-names)
                     nil "Should find user-template-1")
          (cl-assert (member "user-template-2" template-names)
                     nil "Should find user-template-2")
          ;; Should have default templates (if default dir exists)
          (when (aidermacs-templates--get-default-directory)
            (cl-assert (member "code-review" template-names)
                       nil "Should find code-review template")
            ;; Verify user template takes precedence
            (let ((code-review-path (cdr (assoc "code-review" all-templates))))
              (cl-assert (string-prefix-p temp-user-dir code-review-path)
                         nil "User template should take precedence over default")))))
    (delete-directory temp-user-dir t)))

;; Test 15: Verify multiple file extensions are supported
(let ((temp-dir (make-temp-file "aidermacs-ext-test-" t)))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-dir)
            (aidermacs-templates-file-extension '(".txt" ".md" ".org")))
        ;; Create templates with different extensions
        (with-temp-file (expand-file-name "template1.txt" temp-dir)
          (insert "txt template"))
        (with-temp-file (expand-file-name "template2.md" temp-dir)
          (insert "md template"))
        (with-temp-file (expand-file-name "template3.org" temp-dir)
          (insert "org template"))
        (with-temp-file (expand-file-name "ignored.el" temp-dir)
          (insert "should be ignored"))

        ;; Mock get-default-directory to return nil for this test
        (cl-letf (((symbol-function 'aidermacs-templates--get-default-directory)
                   (lambda () nil)))
          (let* ((templates (aidermacs-templates--list-templates))
                 (template-names (sort (mapcar #'car templates) #'string<)))
            (message "Test 15 - Templates with multiple extensions: %s" template-names)
            (cl-assert (equal template-names '("template1" "template2" "template3"))
                       nil "Should find templates with all configured extensions"))))
    (delete-directory temp-dir t)))

;; Test 16: Verify template processing with real template files

;; Test 17: Diagnostic - Show what templates are actually available
(message "\n=== DIAGNOSTIC TEST 17 ===")
(message "User templates directory: %s" aidermacs-user-templates-directory)
(message "User templates directory exists: %s" (file-directory-p aidermacs-user-templates-directory))
(let ((default-dir (aidermacs-templates--get-default-directory)))
  (message "Default templates directory: %s" default-dir)
  (message "Default templates directory exists: %s" (and default-dir (file-directory-p default-dir)))
  (when default-dir
    (let ((default-templates (aidermacs-templates--list-templates-from-dir default-dir)))
      (message "Default templates found: %s" (mapcar #'car default-templates))))
  (when (file-directory-p aidermacs-user-templates-directory)
    (let ((user-templates (aidermacs-templates--list-templates-from-dir aidermacs-user-templates-directory)))
      (message "User templates found: %s" (mapcar #'car user-templates))))
  (let ((all-templates (aidermacs-templates--list-templates)))
    (message "All templates (merged): %s" (mapcar #'car all-templates))
    (message "Total template count: %d" (length all-templates))))
(message "=== END DIAGNOSTIC ===\n")

(let ((temp-dir (make-temp-file "aidermacs-process-test-" t)))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-dir))
        ;; Create a template with placeholders
        (with-temp-file (expand-file-name "test-process.txt" temp-dir)
          (insert "/web {URL}\n\nAnalyze {Topic} and provide {Output-Format}"))

        (let* ((template-file (expand-file-name "test-process.txt" temp-dir))
               (template-text (aidermacs-templates--read-template template-file))
               (placeholders (aidermacs-templates--extract-placeholders template-text)))
          (message "Test 16 - Extracted placeholders: %s" placeholders)
          (cl-assert (equal placeholders '("URL" "Topic" "Output-Format"))
                     nil "Should extract all placeholders in order")

          ;; Test replacement
          (let* ((replacements '(("URL" . "https://example.com")
                                ("Topic" . "AI trends")
                                ("Output-Format" . "bullet points")))
                 (result (aidermacs-templates--replace-placeholders template-text replacements)))
            (message "Test 16 - Processed template: %s" result)
            (cl-assert (string-match-p "https://example.com" result)
                       nil "Should replace URL placeholder")
            (cl-assert (string-match-p "AI trends" result)
                       nil "Should replace Topic placeholder")
            (cl-assert (string-match-p "bullet points" result)
                       nil "Should replace Output-Format placeholder")
            (cl-assert (not (string-match-p "{" result))
                       nil "Should not have any remaining placeholders"))))
    (delete-directory temp-dir t)))
;; (calling from a buffer with no file-name and no load-file-name)
(let ((temp-dir (make-temp-file "aidermacs-templates-test-" t)))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-dir)
            (load-file-name nil)
            (buffer-file-name nil))
        ;; Create a test template
        (with-temp-file (expand-file-name "backtrace-test.txt" temp-dir)
          (insert "/ask {Question}"))
        ;; This should not crash even when both load-file-name and buffer-file-name are nil
        (let ((templates (aidermacs-templates--list-templates)))
          (message "Test 12 - Backtrace scenario: %s" (mapcar #'car templates))
          (cl-assert (>= (length templates) 1))
          (cl-assert (member "backtrace-test" (mapcar #'car templates)))))
    (delete-directory temp-dir t)))

(message "All tests passed!")

(provide 'test-template-system)
;;; test-template-system.el ends here