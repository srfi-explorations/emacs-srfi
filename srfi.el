;;; srfi.el --- Scheme Requests for Implementation browser -*- lexical-binding: t -*-
;;
;; Copyright 2019, 2020 Lassi Kortela
;; SPDX-License-Identifier: MIT
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/srfi-explorations/emacs-srfi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: languages util
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides quick access to Scheme Requests for Implementation (SRFI)
;; documents from within Emacs:
;;
;; * `M-x srfi-list` brings up a *SRFI* buffer listing all SRFIs.
;;
;; * `M-x srfi` does the same, but lets you live-narrow the list by
;;   typing numbers or words into the minibuffer.
;;
;; * The *SRFI* buffer provides one-key commands to visit the SRFI
;;   document, its discussion (mailing list) archive, and its version
;;   control repository. These commands open the right web pages in
;;   your web browser using `browse-url'.
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

(defvar srfi-narrow-keyword nil
  "The current keyword being shown in the *SRFI* buffer.")

(defvar srfi-abstract-directory nil
  "A directory containing all the SRFI abstracts.")

(defvar srfi-source-directory nil
  "A directory containing all the SRFI repos.")

(defun srfi--number-on-line ()
  "Get the number of the SRFI on the current visible line."
  (save-excursion
    (when (invisible-p (point))
      (goto-char (next-single-property-change (point) 'invisible)))
    (or (get-text-property (point) 'srfi-number)
        (error "No SRFI on this line"))))

(defun srfi--goto-first-srfi ()
  "Go to line of first visible SRFI."
  (goto-char (next-single-property-change
              (point-min) 'srfi-number nil (point-max))))

(defun srfi--repository-url (srfi-number)
  "Get the web URL for the version control repository of SRFI-NUMBER."
  (format "https://github.com/scheme-requests-for-implementation/srfi-%d"
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

(defun srfi-browse-repository-url ()
  "Browse version control repository of the SRFI on the current line."
  (interactive)
  (browse-url (srfi--repository-url (srfi--number-on-line))))

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

(defvar srfi-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'srfi-browse-url)
    (define-key map (kbd "a") 'srfi-abstract)
    (define-key map (kbd "d") 'srfi-browse-discussion-url)
    (define-key map (kbd "f") 'srfi-source)
    (define-key map (kbd "j") 'srfi-dired)
    (define-key map (kbd "k") 'srfi-keyword)
    (define-key map (kbd "r") 'srfi-browse-repository-url)
    (define-key map (kbd "s") 'srfi)
    (define-key map (kbd "S") 'srfi-fresh-search)
    (define-key map (kbd "w") 'srfi-browse-website)
    map)
  "Keymap for `srfi-mode'.")

(define-derived-mode srfi-mode special-mode "SRFI"
  "Major mode for browsing the SRFI list.

\\{srfi-mode-map}"
  (setq-local revert-buffer-function 'srfi-revert)
  (setq-local font-lock-defaults
              '((srfi-mode-font-lock-keywords) nil nil nil nil))
  (unless (equal (buffer-name) "*SRFI*")
    (message (concat "Note: srfi-mode is only meant for the *SRFI* buffer. "
                     "Try M-x srfi."))))

(defun srfi--narrow (query)
  "Internal function to narrow the *SRFI* buffer based on QUERY."
  (with-current-buffer (get-buffer "*SRFI*")
    (with-selected-window (get-buffer-window (current-buffer))
      (widen)
      (let ((inhibit-read-only t) (case-fold-search t))
        (goto-char (point-min))
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
        (recenter 0))
      (srfi--goto-first-srfi))))

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
       "Scheme Requests for Implementation"
       (if (not srfi-narrow-keyword) ""
         (concat " (" srfi-narrow-keyword ")"))
       "\n"
       "\n"
       "RET: browse SRFI document | "
       "d: discussion | "
       "r: repo | "
       "s: search | "
       "w: website\n"
       "\n")
      (let ((count (truncate (length srfi-data) 3)))
        (dotimes (i count)
          (let* ((number (- count 1 i))
                 (base   (* number 3))
                 (year   (elt srfi-data base))
                 (status (elt srfi-data (+ base 1)))
                 (title  (elt srfi-data (+ base 2)))
                 (beg    (point)))
            (when (or (not srfi-narrow-keyword)
                      (member number (cdr (assoc srfi-narrow-keyword
                                                 srfi-data-keywords))))
              (insert (format "SRFI %3d: %s (%s)\n" number title
                              (cl-case status
                                ((final) year)
                                ((withdrawn) (format "%S, withdrawn" year))
                                (t status))))
              (let ((end (point)))
                (put-text-property beg end 'srfi-number number)))))))
    (srfi--narrow srfi-narrow-query)))

;;;###autoload
(defun srfi-list ()
  "Show the *SRFI* buffer."
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*SRFI*"))
    (switch-to-buffer-other-window "*SRFI*"))
  (srfi-revert))

;;;###autoload
(defun srfi ()
  "Show the *SRFI* buffer and live-narrow it from the minibuffer."
  (interactive)
  (srfi-list)
  (minibuffer-with-setup-hook
      (lambda () (add-hook 'after-change-functions #'srfi--narrow-minibuffer
                           nil 'local))
    (setq srfi-narrow-query (read-string "SRFI: " srfi-narrow-query))))

(defun srfi-fresh-search ()
  "Show the *SRFI* buffer and live-narrow it from scratch."
  (interactive)
  (setq srfi-narrow-query "")
  (srfi))

(defun srfi-keyword (keyword)
  "Show the *SRFI* buffer and narrow it to a paricular KEYWORD."
  (interactive (list (completing-read
                      "Narrow SRFIs to keyword: " srfi-data-keywords
                      nil t nil nil (list nil))))
  (setq srfi-narrow-keyword keyword)
  (unless (= 0 (length srfi-narrow-query))
    (message "NOTE: The SRFI search filter is still active as well."))
  (srfi-list))

(defun srfi-dired ()
  "Open directory containing SRFI document in Dired."
  (interactive)
  (assert srfi-source-directory
          nil
          "You must set `srfi-source-directory' first.")
  (dired
   (substitute-in-file-name
    (format "%s/srfi-%d/" srfi-source-directory (srfi--number-on-line)))))

(defun srfi-source ()
  "Open SRFI document."
  (interactive)
  (assert srfi-source-directory
          nil
          "You must set `srfi-source-directory' first.")
  (let ((srfi (srfi--number-on-line)))
    (find-file
     (substitute-in-file-name
      (format "%s/srfi-%d/srfi-%d.html" srfi-source-directory srfi srfi)))))

(defun srfi-abstract ()
  "Open SRFI abstract document."
  (interactive)
  (assert srfi-source-directory
          nil
          "You must set `srfi-abstract-directory' first.")
  (find-file
   (substitute-in-file-name
    (format "%s/%d.html" srfi-abstract-directory (srfi--number-on-line)))))

(provide 'srfi)

;;; srfi.el ends here
