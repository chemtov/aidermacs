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

;; --- Filesystem tests ---
(let ((temp-dir (make-temp-directory "aidermacs-templates-test-")))
  (unwind-protect
      (let ((aidermacs-user-templates-directory temp-dir)
            (aidermacs-templates-file-extension '(".txt" ".md")))
        (flet ((aidermacs-templates--get-default-directory ()
                 ;; Return a non-existent dir to isolate user templates
                 (expand-file-name "non-existent" temp-dir)))

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


(message "All tests passed!")

(provide 'test-template-system)
;;; test-template-system.el ends here
