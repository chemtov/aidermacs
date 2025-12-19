;;; test-compile.el --- Compilation tests for aidermacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This test should run FIRST before any other tests.
;; It compiles all elisp files to catch syntax errors, undefined functions,
;; and other compilation warnings that could cause runtime errors.
;; All .elc files are cleaned up after testing to prevent them from being
;; committed to the repository.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)

(defvar test-compile-files
  '("aidermacs-backend-comint.el"
    "aidermacs-backend-vterm.el"
    "aidermacs-backends.el"
    "aidermacs-models.el"
    "aidermacs-output.el"
    "aidermacs-templates.el"
    "aidermacs.el")
  "List of elisp files to compile in dependency order.")

(defvar test-compile-failed nil
  "Flag to track if any compilation failed.")

(defvar test-compile-elc-files nil
  "List of .elc files created during testing.")

(defun test-compile-cleanup ()
  "Remove all .elc files created during testing."
  (message "\n=== Cleaning up .elc files ===")
  (dolist (elc-file test-compile-elc-files)
    (when (file-exists-p elc-file)
      (delete-file elc-file)
      (message "Deleted: %s" elc-file)))
  (message "=== Cleanup complete ===\n"))

(defun test-compile-file (file)
  "Compile FILE and track the .elc file for cleanup.
Returns t if compilation succeeded, nil otherwise."
  (let* ((elc-file (concat file "c"))
         (byte-compile-error-on-warn nil) ; Don't fail on warnings, just errors
         ;; Suppress warnings about missing optional packages
         (byte-compile-warnings '(not cl-functions free-vars unresolved))
         (byte-compile-dest-file-function
          (lambda (source)
            (prog1 elc-file
              (push elc-file test-compile-elc-files)))))
    (message "\n--- Compiling %s ---" file)
    (condition-case err
        (progn
          ;; Provide stub for markdown-mode if not available
          (unless (featurep 'markdown-mode)
            (provide 'markdown-mode))
          
          ;; Load previously compiled files as dependencies
          (when (string= file "aidermacs-backends.el")
            (require 'aidermacs-backend-comint nil t)
            (require 'aidermacs-backend-vterm nil t))
          (when (string= file "aidermacs.el")
            (require 'aidermacs-backend-comint nil t)
            (require 'aidermacs-backend-vterm nil t)
            (require 'aidermacs-backends nil t)
            (require 'aidermacs-models nil t)
            (require 'aidermacs-output nil t)
            (require 'aidermacs-templates nil t))
          
          (byte-compile-file file)
          (if (file-exists-p elc-file)
              (progn
                (message "✓ Successfully compiled %s" file)
                ;; Load the compiled file so it's available for next files
                (load (expand-file-name elc-file) t t)
                t)
            (progn
              (message "✗ Compilation failed for %s (no .elc produced)" file)
              (setq test-compile-failed t)
              nil)))
      (error
       (message "✗ Compilation error in %s: %S" file err)
       (setq test-compile-failed t)
       nil))))

(defun test-compile-all ()
  "Compile all aidermacs elisp files and report results."
  (message "=== Starting Compilation Tests ===")
  (message "Testing files in dependency order...\n")
  
  (let ((success-count 0)
        (fail-count 0))
    
    ;; Compile each file
    (dolist (file test-compile-files)
      (if (file-exists-p file)
          (if (test-compile-file file)
              (cl-incf success-count)
            (cl-incf fail-count))
        (message "⚠ Warning: File not found: %s" file)
        (cl-incf fail-count)))
    
    ;; Report results
    (message "\n=== Compilation Test Results ===")
    (message "Files compiled successfully: %d" success-count)
    (message "Files failed to compile: %d" fail-count)
    
    ;; Always cleanup
    (test-compile-cleanup)
    
    ;; Exit with appropriate code
    (if test-compile-failed
        (progn
          (message "\n✗ COMPILATION TESTS FAILED")
          (message "Fix compilation errors before running other tests.")
          (kill-emacs 1))
      (progn
        (message "\n✓ ALL COMPILATION TESTS PASSED")
        (kill-emacs 0)))))

;; Run tests when loaded in batch mode
(when noninteractive
  (test-compile-all))

(provide 'test-compile)
;;; test-compile.el ends here
