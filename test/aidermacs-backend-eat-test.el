;;; aidermacs-backend-eat-test.el --- Tests for Eat backend -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Aidermacs Contributors
;; Keywords: tests

;;; Commentary:

;; Tests for the Eat backend implementation.
;; Mocks external dependencies (Eat, processes) to test logic in isolation.

;;; Code:

(require 'ert)
(require 'eat)
(require 'aidermacs-backend-eat)

;; Mock external variables
(defvar aidermacs-prompt-regexp "> ")

;; Mock variables
(defvar aidermacs-eat-test--sent-string nil)
(defvar aidermacs-eat-test--process-mock nil)
(defvar aidermacs-eat-test--buffer nil)
;; Mock functions
(defun aidermacs-eat-test--mock-start-process (&rest _args)
  aidermacs-eat-test--process-mock)

(defun aidermacs-eat-test--mock-process-send-string (_proc string)
  (setq aidermacs-eat-test--sent-string string))

(defun aidermacs-eat-test--mock-aidermacs--store-output (output)
  (put 'aidermacs--store-output 'last-call output))

(defun aidermacs-eat-test--mock-aidermacs--cleanup-temp-buffers ()
  (put 'aidermacs--cleanup-temp-buffers 'called t))

(defun aidermacs-eat-test--mock-aidermacs--detect-edited-files ()
  nil)

;;; Tests

(ert-deftest aidermacs-eat-test-run-eat ()
  "Test initialization of Eat backend."
  (let ((eat-exec-function (symbol-function 'eat-exec)))
    
    (cl-letf (((symbol-function 'eat-exec) (lambda (buffer name program env args)
                                             (with-current-buffer buffer
                                               (eat-mode)
                                               (setq aidermacs-eat--terminal (make-hash-table))))))
      
      (let ((buffer (aidermacs-run-eat "aider" '("--model" "gpt-4") "*test-aider-eat*")))
        
        (unwind-protect
            (with-current-buffer buffer
              ;; Verify buffer setup
              (should (string= (buffer-name buffer) "*test-aider-eat*"))
              (should (derived-mode-p 'eat-mode))
              (should aidermacs-eat-mode)
              
              ;; Verify formatter setup
              (should aidermacs-eat--use-faces)
              (should (equal aidermacs-eat--output-accumulator ""))
              (should aidermacs--ready))
          
          ;; Cleanup
          (kill-buffer buffer))))))

(ert-deftest aidermacs-eat-test-send-command ()
  "Test sending commands to Eat process."
  (let ((sent-chars nil))
    
    (with-temp-buffer
      (eat-mode)
      (aidermacs-eat-mode 1)
      
      (cl-letf (((symbol-function 'eat-self-input) (lambda (count char)
                                                     (push char sent-chars)))
                ((symbol-function 'aidermacs--process-message-if-multi-line) (lambda (s) s))
                ((symbol-function 'aidermacs--command-may-edit-files) (lambda (_) nil)))
        
        ;; Test sending simple command
        (aidermacs--send-command-eat (current-buffer) "help")
        (should (equal (reverse sent-chars) (append (string-to-list "help") (list ?\r))))
        (should-not aidermacs--ready)))))

(ert-deftest aidermacs-eat-test-process-filter-accumulation ()
  "Test output accumulation and filtering."
  (with-temp-buffer
    (aidermacs-eat-mode 1)
    (setq aidermacs-eat--output-accumulator "")
    
    (cl-letf (((symbol-function 'aidermacs--is-aidermacs-buffer-p) (lambda () t))
              ((symbol-function 'aidermacs-eat-filter-function) (lambda (s) (concat "FORMATTED:" s))))
      
      ;; 1. Receive partial output
      (let ((formatted (aidermacs-eat--process-filter "Part 1 ")))
        (should (string= formatted "FORMATTED:Part 1 "))
        (should (string= aidermacs-eat--output-accumulator "Part 1 ")))
      
      ;; 2. Receive rest of output without prompt
      (let ((formatted (aidermacs-eat--process-filter "Part 2\n")))
        (should (string= formatted "FORMATTED:Part 2\n"))
        (should (string= aidermacs-eat--output-accumulator "Part 1 Part 2\n"))))))

(ert-deftest aidermacs-eat-test-process-filter-prompt-detection ()
  "Test prompt detection in output stream."
  (with-temp-buffer
    (aidermacs-eat-mode 1)
    (setq aidermacs-eat--output-accumulator "")
    (setq aidermacs-prompt-regexp "> $") 
    
    (cl-letf (((symbol-function 'aidermacs--is-aidermacs-buffer-p) (lambda () t))
              ((symbol-function 'aidermacs-eat-filter-function) (lambda (s) s))
              ((symbol-function 'aidermacs--store-output) #'aidermacs-eat-test--mock-aidermacs--store-output)
              ((symbol-function 'aidermacs--cleanup-temp-buffers) #'aidermacs-eat-test--mock-aidermacs--cleanup-temp-buffers)
              ((symbol-function 'aidermacs--detect-edited-files) #'aidermacs-eat-test--mock-aidermacs--detect-edited-files))
      
      ;; Send output containing prompt
      (aidermacs-eat--process-filter "Some output\n> ")
      
      ;; Verify prompt actions triggered
      (should aidermacs--ready)
      (should (equal (get 'aidermacs--store-output 'last-call) "Some output\n> "))
      (should (get 'aidermacs--cleanup-temp-buffers 'called))
      (should (string= aidermacs-eat--output-accumulator "")))))

(ert-deftest aidermacs-eat-test-cleanup ()
  "Test resource cleanup."
  (let ((aidermacs-eat-test--process-mock (make-process :name "test" :command '("sleep" "10")))
        (start-process-function (symbol-function 'start-process)))
    
    (cl-letf (((symbol-function 'start-process) #'aidermacs-eat-test--mock-start-process)
              ((symbol-function 'delete-process) (lambda (p) (setq aidermacs-eat-test--process-mock nil))))
      
      (let ((buffer (aidermacs-run-eat "aider" nil "*test-aider-eat-cleanup*")))
        (with-current-buffer buffer
           (let ((term aidermacs-eat--terminal))
             (should (eat-term-live-p term))
             (aidermacs-eat--cleanup)
             (should-not (eat-term-live-p term))
             (should (null aidermacs-eat-test--process-mock))))
        (kill-buffer buffer)))))

(provide 'aidermacs-backend-eat-test)
;;; aidermacs-backend-eat-test.el ends here
