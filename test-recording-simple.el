;;; test-recording-simple.el --- Simple tests for recording functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal tests that don't require full aidermacs loading

;;; Code:

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
(let ((result (aidermacs--asciinema-available-p)))
  (message "Test 1 - asciinema available: %s" (if result "yes" "no"))
  (cl-assert (or (null result) (stringp result))))

;; Test 2: Generate recording filename
(let ((filename (aidermacs--generate-recording-filename)))
  (message "Test 2 - Generated filename: %s" filename)
  (cl-assert (stringp filename))
  (cl-assert (string-match-p "\\.cast$" filename))
  (cl-assert (string-match-p "-[0-9]\\{8\\}-[0-9]\\{6\\}\\.cast$" filename)))

;; Test 3: Verify recording directory is created
(let* ((test-dir (make-temp-file "aidermacs-recording-test-" t))
       (aidermacs-recording-directory test-dir))
  (unwind-protect
      (let ((filename (aidermacs--generate-recording-filename)))
        (message "Test 3 - Recording directory created: %s" test-dir)
        (cl-assert (file-directory-p test-dir))
        (cl-assert (string-prefix-p test-dir filename)))
    (delete-directory test-dir t)))

;; Test 4: Verify filename format includes project name
(let* ((aidermacs-recording-directory (make-temp-file "aidermacs-rec-" t)))
  (unwind-protect
      (let ((filename (aidermacs--generate-recording-filename)))
        (message "Test 4 - Filename with project name: %s" filename)
        (cl-assert (string-match-p "test-project-" filename)))
    (delete-directory aidermacs-recording-directory t)))

;; Test 5: Verify recording file path is absolute
(let ((filename (aidermacs--generate-recording-filename)))
  (message "Test 5 - Filename is absolute: %s" filename)
  (cl-assert (file-name-absolute-p filename)))

;; Test 6: Verify multiple calls generate unique filenames
(let ((filename1 (aidermacs--generate-recording-filename)))
  (sit-for 1) ; Wait 1 second to ensure different timestamp
  (let ((filename2 (aidermacs--generate-recording-filename)))
    (message "Test 6 - Unique filenames: %s vs %s" filename1 filename2)
    (cl-assert (not (equal filename1 filename2)))))

(message "All recording system tests passed!")

(provide 'test-recording-simple)
;;; test-recording-simple.el ends here
