;;; test-eval.el --- Eval-buffer tests for aidermacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This test evaluates each file to catch macro expansion errors and
;; runtime evaluation issues that byte-compilation doesn't detect.
;; Examples: transient-define-prefix macro errors, defcustom issues, etc.
;;
;; Dependencies are installed via package.el on first run and cached.

;;; Code:

(require 'package)

(defvar test-eval-files
  '("aidermacs-backend-comint.el"
    "aidermacs-backend-vterm.el"
    "aidermacs-backends.el"
    "aidermacs-models.el"
    "aidermacs-output.el"
    "aidermacs-templates.el"
    "aidermacs.el")
  "List of elisp files to evaluate in dependency order.")

(defvar test-eval-dependencies
  '(transient compat markdown-mode)
  "List of packages required for evaluation.")

(defvar test-eval-failed nil
  "Flag to track if any evaluation failed.")

(defun test-eval-setup-packages ()
  "Ensure required packages are installed."
  (message "\n=== Setting up package dependencies ===")

  ;; Initialize package.el
  (setq package-user-dir (expand-file-name ".test-elpa" default-directory))
  (package-initialize)

  ;; Add MELPA if not already present
  (unless (assoc "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

  ;; Refresh package list if needed
  (unless package-archive-contents
    (message "Refreshing package archives...")
    (package-refresh-contents))

  ;; Install missing dependencies
  (dolist (pkg test-eval-dependencies)
    (unless (package-installed-p pkg)
      (message "Installing %s..." pkg)
      (package-install pkg)))

  (message "=== Package setup complete ===\n"))

(defun test-eval-file (file)
  "Evaluate FILE and report any errors.
Returns t if evaluation succeeded, nil otherwise."
  (message "\n--- Evaluating %s ---" file)
  (condition-case err
      (progn
        ;; Load the file
        (load-file file)
        (message "✓ Successfully evaluated %s" file)
        t)
    (error
     (message "✗ Evaluation error in %s: %S" file err)
     (setq test-eval-failed t)
     nil)))

(defun test-eval-all ()
  "Evaluate all aidermacs elisp files and report results."
  (message "=== Starting Eval-Buffer Tests ===")
  (message "These tests catch macro expansion and runtime errors\n")

  ;; Setup packages
  (test-eval-setup-packages)

  (let ((success-count 0)
        (fail-count 0))

    ;; Evaluate each file
    (dolist (file test-eval-files)
      (if (file-exists-p file)
          (if (test-eval-file file)
              (cl-incf success-count)
            (cl-incf fail-count))
        (message "⚠ Warning: File not found: %s" file)
        (cl-incf fail-count)))

    ;; Report results
    (message "\n=== Eval-Buffer Test Results ===")
    (message "Files evaluated successfully: %d" success-count)
    (message "Files failed to evaluate: %d" fail-count)

    ;; Exit with appropriate code
    (if test-eval-failed
        (progn
          (message "\n✗ EVAL-BUFFER TESTS FAILED")
          (message "Fix evaluation errors (macro expansions, runtime issues, etc.)")
          (kill-emacs 1))
      (progn
        (message "\n✓ ALL EVAL-BUFFER TESTS PASSED")
        (kill-emacs 0)))))

;; Run tests when loaded in batch mode
(when noninteractive
  (test-eval-all))

(provide 'test-eval)
;;; test-eval.el ends here
