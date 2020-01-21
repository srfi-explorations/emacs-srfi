;;; srfi.el --- Scheme Requests for Implementation
;;
;; SPDX-FileCopyrightText: 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-srfi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: languages util
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides commands to list and visit SRFIs from Emacs.
;;
;;; Code:

(require 'browse-url)

(require 'srfi-data)

(defconst srfi-mode-font-lock-keywords
  `(("^SRFI +\\([0-9]+\\): \\(.*?\\) (draft)$"
     (1 font-lock-keyword-face)
     (2 font-lock-preprocessor-face))
    ("^SRFI +\\([0-9]+\\): \\(.*?\\) ([0-9]\\{4\\})$"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("^SRFI +\\([0-9]+\\): \\(.*?\\) ([0-9]\\{4\\}, withdrawn)$"
     (1 font-lock-keyword-face)
     (2 font-lock-comment-face))))

(defvar srfi-narrow-query ""
  "The current narrowing text in effect in the *SRFI* buffer.")

(defun srfi--number-on-line ()
  "Get the number of the SRFI on the current visible line."
  (save-excursion
    (when (invisible-p (point))
      (goto-char (next-single-property-change (point) 'invisible)))
    (or (get-text-property (point) 'srfi-number)
        (error "No SRFI on this line"))))

(defun srfi--version-control-url (srfi-number)
  "Get the web URL for the version control repository of SRFI-NUMBER."
  (format "https://github.com/scheme-requests-for-implementation/srfi-%d/"
          srfi-number))

(defun srfi--discussion-url (srfi-number)
  "Get the web URL for the mailing list archive of SRFI-NUMBER."
  (format "https://srfi-email.schemers.org/srfi-%d/" srfi-number))

(defun srfi--landing-page-url (srfi-number)
  "Get the web URL for the landing page of SRFI-NUMBER."
  (format "https://srfi.schemers.org/srfi-%d/"
          srfi-number))

(defun srfi--document-url (srfi-number)
  "Get the web URL for the SRFI document SRFI-NUMBER."
  (format "https://srfi.schemers.org/srfi-%d/srfi-%d.html"
          srfi-number srfi-number))

(defun srfi-browse-version-control-url ()
  "Browse version control repository of the SRFI on the current line."
  (interactive)
  (browse-url (srfi--version-control-url (srfi--number-on-line))))

(defun srfi-browse-discussion-url ()
  "Browse mailing list archive of the SRFI on the current line."
  (interactive)
  (browse-url (srfi--discussion-url (srfi--number-on-line))))

(defun srfi-browse-landing-page-url ()
  "Browse landing page of the SRFI on the current line."
  (interactive)
  (browse-url (srfi--landing-page-url (srfi--number-on-line))))

(defun srfi-browse-url ()
  "Browse the SRFI document on the current line."
  (interactive)
  (browse-url (srfi--document-url (srfi--number-on-line))))

(defun srfi-browse-website ()
  "Browse the home page of the SRFI specification process."
  (interactive)
  (browse-url "https://srfi.schemers.org/"))

(define-derived-mode srfi-mode special-mode "SRFI"
  "Major mode for browsing the SRFI list.

\\{srfi-mode-map}"
  (setq-local revert-buffer-function 'srfi-revert)
  (setq-local font-lock-defaults
              '((srfi-mode-font-lock-keywords) nil nil nil nil)))

(define-key srfi-mode-map (kbd "RET") 'srfi-browse-url)
(define-key srfi-mode-map (kbd "d") 'srfi-browse-discussion-url)
(define-key srfi-mode-map (kbd "r") 'srfi-browse-version-control-url)
(define-key srfi-mode-map (kbd "s") 'srfi)
(define-key srfi-mode-map (kbd "w") 'srfi-browse-website)

(defun srfi--narrow (query)
  "Internal function to narrow the *SRFI* buffer based on QUERY."
  (with-current-buffer (get-buffer "*SRFI*")
    (with-selected-window (get-buffer-window (current-buffer))
      (widen)
      (let ((inhibit-read-only t) (case-fold-search t))
        (while (< (goto-char (next-single-property-change
                              (point) 'srfi-number nil (point-max)))
                  (point-max))
          (let ((beg (point)) (end (1+ (point-at-eol))))
            (get-text-property (point) 'srfi-number)
            (remove-text-properties beg end '(invisible))
            (unless (looking-at (concat "^.*?" (regexp-quote query)))
              (put-text-property beg end 'invisible 'srfi-narrow)))))
      (goto-char (point-min))
      (let ((recenter-redisplay nil))
        (recenter 0)))))

(defun srfi--narrow-minibuffer (&rest _ignored)
  "Internal function to narrow the *SRFI* buffer."
  (srfi--narrow (minibuffer-contents)))

(defun srfi-revert (&optional _arg _noconfirm)
  "(Re-)initialize the *SRFI* buffer."
  (with-current-buffer (get-buffer-create "*SRFI*")
    (cl-assert (null (buffer-file-name)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (srfi-mode)
      (insert
       "Scheme Requests for Implementation\n"
       "\n"
       "RET: browse SRFI document, "
       "r: repo, "
       "d: discussion, "
       "s: search, "
       "w: website\n"
       "\n")
      (dolist (srfi (reverse srfi-data))
        (cl-destructuring-bind (number status title year) srfi
          (let ((beg (point)))
            (insert (format "SRFI %3d: %s (%s)\n" number title
                            (cl-case status
                              ((final) year)
                              ((withdrawn) (format "%S, withdrawn" year))
                              (t status))))
            (let ((end (point)))
              (put-text-property beg end 'srfi-number number))))))
    (srfi--narrow srfi-narrow-query)))

(defun srfi-list ()
  "Show the *SRFI* buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*SRFI*"))
      (srfi-revert)
    (with-displayed-buffer-window
     "*SRFI*" (list #'display-buffer-pop-up-window) nil
     (srfi-revert))))

(defun srfi ()
  "Show the *SRFI* buffer and live-narrow it from the minibuffer."
  (interactive)
  (srfi-list)
  (minibuffer-with-setup-hook
      (lambda () (add-hook 'after-change-functions #'srfi--narrow-minibuffer
                           nil 'local))
    (setq srfi-narrow-query (read-string "SRFI: " srfi-narrow-query)))
  (unless (eq (current-buffer) (get-buffer "*SRFI*"))
    (switch-to-buffer-other-window "*SRFI*"))
  (goto-char (next-single-property-change
              (point-min) 'srfi-number nil (point-max))))

(provide 'srfi)

;;; srfi.el ends here
