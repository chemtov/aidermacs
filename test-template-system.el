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
(let ((template "URL: {Enter-URL}, Action: {What-to-do-with-it}"))
  (let ((placeholders (aidermacs-templates--extract-placeholders template)))
    (message "Test 5 - Complex names: %s" placeholders)
    (cl-assert (equal placeholders '("Enter-URL" "What-to-do-with-it")))))

(message "All tests passed!")

(provide 'test-template-system)
;;; test-template-system.el ends here
