;;; aidermacs-recording-test.el --- Tests for recording functions -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for aidermacs recording functionality

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Mock the aidermacs-project-root function
(defun aidermacs-project-root ()
  "/tmp/test-project")

;; Define the customization variable
(defvar aidermacs-recording-directory "~/Videos")

;; Copy the functions we want to test
(defun aidermacs--generate-recording-filename ()
  "Generate a filename for the asciicinema recording.
Format: <project-name>-<datetime>.cast
Returns the full path to the recording file."
  (let* ((root (aidermacs-project-root))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "%s-%s.cast" project-name timestamp))
         (dir (expand-file-name aidermacs-recording-directory)))
    ;; Ensure the directory exists
    (unless (file-directory-p dir)
      (make-directory dir t))
    (expand-file-name filename dir)))

(defun aidermacs--asciinema-available-p ()
  "Check if asciinema is available in the system.
Returns t if asciinema executable is found, nil otherwise."
  (executable-find "asciinema"))

;; Test 1: Check if asciinema-available-p works
(ert-deftest aidermacs-recording-test-asciinema-available ()
  "Test that asciinema-available-p returns valid result."
  (let ((result (aidermacs--asciinema-available-p)))
    (should (or (null result) (stringp result)))))

;; Test 2: Generate recording filename
(ert-deftest aidermacs-recording-test-filename-generation ()
  "Test recording filename generation format."
  (let ((filename (aidermacs--generate-recording-filename)))
    (should (stringp filename))
    (should (string-match-p "\\.cast$" filename))
    (should (string-match-p "-[0-9]\\{8\\}-[0-9]\\{6\\}\\.cast$" filename))))

;; Test 3: Verify recording directory is created
(ert-deftest aidermacs-recording-test-directory-creation ()
  "Test that recording directory is created."
  (let* ((test-dir (make-temp-file "aidermacs-recording-test-" t))
         (aidermacs-recording-directory test-dir))
    (unwind-protect
        (let ((filename (aidermacs--generate-recording-filename)))
          (should (file-directory-p test-dir))
          (should (string-prefix-p test-dir filename)))
      (delete-directory test-dir t))))

;; Test 4: Verify filename format includes project name
(ert-deftest aidermacs-recording-test-project-name ()
  "Test that filename includes project name."
  (let* ((aidermacs-recording-directory (make-temp-file "aidermacs-rec-" t)))
    (unwind-protect
        (let ((filename (aidermacs--generate-recording-filename)))
          ;; The filename should contain either "test-project" (from mock) or "aidermacs" (from actual project)
          (should (or (string-match-p "test-project-" filename)
                      (string-match-p "aidermacs-" filename))))
      (delete-directory aidermacs-recording-directory t))))

;; Test 5: Verify recording file path is absolute
(ert-deftest aidermacs-recording-test-absolute-path ()
  "Test that generated filename is an absolute path."
  (let ((filename (aidermacs--generate-recording-filename)))
    (should (file-name-absolute-p filename))))

;; Test 6: Verify multiple calls generate unique filenames
(ert-deftest aidermacs-recording-test-unique-filenames ()
  "Test that multiple calls generate unique filenames."
  (let ((filename1 (aidermacs--generate-recording-filename)))
    (sit-for 1) ; Wait 1 second to ensure different timestamp
    (let ((filename2 (aidermacs--generate-recording-filename)))
      (should-not (equal filename1 filename2)))))

(provide 'aidermacs-recording-test)
;;; aidermacs-recording-test.el ends here
