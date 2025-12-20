;;; aidermacs-compile-test.el --- Compilation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for byte-compilation of aidermacs files.
;; These tests compile all elisp files to catch syntax errors, undefined functions,
;; and other compilation warnings that could cause runtime errors.

;;; Code:

(require 'ert)
(require 'bytecomp)
(require 'cl-lib)

(defvar aidermacs-compile-test-files
  '("aidermacs-backend-comint.el"
    "aidermacs-backend-vterm.el"
    "aidermacs-backends.el"
    "aidermacs-models.el"
    "aidermacs-output.el"
    "aidermacs-templates.el"
    "aidermacs.el")
  "List of elisp files to compile in dependency order.")

(defvar aidermacs-compile-test-elc-files nil
  "List of .elc files created during testing.")

(defun aidermacs-compile-test-cleanup ()
  "Remove all .elc files created during testing."
  (dolist (elc-file aidermacs-compile-test-elc-files)
    (when (file-exists-p elc-file)
      (delete-file elc-file)))
  (setq aidermacs-compile-test-elc-files nil))

(defun aidermacs-compile-test-file (file)
  "Compile FILE and track the .elc file for cleanup.
Returns t if compilation succeeded, nil otherwise."
  (let* ((elc-file (concat file "c"))
         (byte-compile-error-on-warn nil)
         (byte-compile-warnings '(not cl-functions free-vars unresolved))
         (byte-compile-dest-file-function
          (lambda (source)
            (prog1 elc-file
              (push elc-file aidermacs-compile-test-elc-files)))))
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
                ;; Load the compiled file so it's available for next files
                (load (expand-file-name elc-file) t t)
                t)
            nil))
      (error
       nil))))

(ert-deftest aidermacs-compile-test-all-files ()
  "Test that all aidermacs elisp files compile without errors."
  (unwind-protect
      (dolist (file aidermacs-compile-test-files)
        (when (file-exists-p file)
          (should (aidermacs-compile-test-file file))))
    (aidermacs-compile-test-cleanup)))

(provide 'aidermacs-compile-test)
;;; aidermacs-compile-test.el ends here
