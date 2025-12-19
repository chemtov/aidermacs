;;; aidermacs-eval-test.el --- Eval-buffer tests -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests that evaluate each file to catch macro expansion errors and
;; runtime evaluation issues that byte-compilation doesn't detect.
;; Examples: transient-define-prefix macro errors, defcustom issues, etc.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar aidermacs-eval-test-files
  '("aidermacs-backend-comint.el"
    "aidermacs-backend-vterm.el"
    "aidermacs-backends.el"
    "aidermacs-models.el"
    "aidermacs-output.el"
    "aidermacs-templates.el"
    "aidermacs.el")
  "List of elisp files to evaluate in dependency order.")

(defun aidermacs-eval-test-file (file)
  "Evaluate FILE and return t if successful, nil otherwise."
  (condition-case err
      (progn
        (load-file file)
        t)
    (error
     (message "Evaluation error in %s: %S" file err)
     nil)))

(ert-deftest aidermacs-eval-test-backend-comint ()
  "Test that aidermacs-backend-comint.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs-backend-comint.el")))

(ert-deftest aidermacs-eval-test-backend-vterm ()
  "Test that aidermacs-backend-vterm.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs-backend-vterm.el")))

(ert-deftest aidermacs-eval-test-backends ()
  "Test that aidermacs-backends.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs-backends.el")))

(ert-deftest aidermacs-eval-test-models ()
  "Test that aidermacs-models.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs-models.el")))

(ert-deftest aidermacs-eval-test-output ()
  "Test that aidermacs-output.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs-output.el")))

(ert-deftest aidermacs-eval-test-templates ()
  "Test that aidermacs-templates.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs-templates.el")))

(ert-deftest aidermacs-eval-test-main ()
  "Test that aidermacs.el evaluates without errors."
  (should (aidermacs-eval-test-file "aidermacs.el")))

(provide 'aidermacs-eval-test)
;;; aidermacs-eval-test.el ends here
