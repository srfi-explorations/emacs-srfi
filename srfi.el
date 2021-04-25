;;; srfi.el --- Scheme Requests for Implementation browser -*- lexical-binding: t -*-
;;
;; Copyright 2019, 2020, 2021 Lassi Kortela
;; Copyright 2020 Arthur A. Gleckler
;; SPDX-License-Identifier: MIT
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/srfi-explorations/emacs-srfi
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))
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
;;   your web browser using `browse-url'. The standard variable
;;   `browse-url-browser-function' can be used to control which web
;;   browser is used to open what pages.
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

(defun srfi--parse-number (string)
  "Internal function to parse a SRFI number from STRING.

Strict rules: base 10, no leading zeros, no whitespace."
  (save-match-data
    (if (string= string "0") 0
        (and (string-match "^[1-9][0-9]*$" string)
             (string-to-number string 10)))))

(defun srfi--number-title (srfi-number)
  "Get the title corresponding to the given SRFI-NUMBER as string."
  (elt srfi-data (+ 2 (* 3 srfi-number))))

(defun srfi--number-on-line ()
  "Get the number of the SRFI on the current line."
  (or (get-text-property (point) 'srfi-number)
      (error "No SRFI on this line")))

(defun srfi--goto-first-srfi ()
  "Scroll window to top and go to line of first SRFI."
  (goto-char (point-min))
  (let ((recenter-redisplay nil))
    (recenter 0))
  (goto-char (next-single-property-change
              (point-min) 'srfi-number nil (point-max))))

(defun srfi--goto-number (number)
  "Go to line of SRFI with the given NUMBER."
  (let* ((buffer (get-buffer "*SRFI*"))
         (window (and buffer (get-buffer-window buffer))))
    (when window
      (with-selected-window window
        (let ((here (point-min)) (number-here nil))
          (while (and (not (equal number number-here))
                      (setq here (next-single-property-change
                                  here 'srfi-number nil (point-max))))
            (goto-char here)
            (setq number-here (get-text-property here 'srfi-number)))
          (when (equal number number-here) number))))))

(defun srfi--repository-url (srfi-number)
  "Get the web URL for the version control repository of SRFI-NUMBER."
  (format "https://github.com/scheme-requests-for-implementation/srfi-%d"
          srfi-number))

(defun srfi--discussion-url (srfi-number)
  "Get the web URL for the mailing list archive of SRFI-NUMBER."
  (format "https://srfi-email.schemers.org/srfi-%d/" srfi-number))

(defun srfi--discussion-email-address (srfi-number)
  "Get the email address for the mailing list of SRFI-NUMBER."
  (format "srfi-%d@srfi.schemers.org" srfi-number))

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

(defun srfi-browse-document-url ()
  "Browse the SRFI document on the current line."
  (interactive)
  (browse-url (srfi--document-url (srfi--number-on-line))))

(defun srfi-compose-mail ()
  "Write an email to the mailing list of the SRFI on the current line.

Use `mail-user-agent' for customization."
  (interactive)
  (let ((number (srfi--number-on-line)))
    (compose-mail (srfi--discussion-email-address number)
                  (srfi--number-title number))))

(defun srfi-browse-website-url ()
  "Browse the home page of the SRFI specification process."
  (interactive)
  (browse-url "https://srfi.schemers.org/"))

(defvar srfi-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'srfi-browse-document-url)
    (define-key map (kbd "a") 'srfi-abstract)
    (define-key map (kbd "d") 'srfi-browse-discussion-url)
    (define-key map (kbd "f") 'srfi-source)
    (define-key map (kbd "j") 'srfi-dired)
    (define-key map (kbd "k") 'srfi-keyword)
    (define-key map (kbd "l") 'srfi-browse-landing-page-url)
    (define-key map (kbd "m") 'srfi-compose-mail)
    (define-key map (kbd "r") 'srfi-browse-repository-url)
    (define-key map (kbd "s") 'srfi-search)
    (define-key map (kbd "S") 'srfi-fresh-search)
    (define-key map (kbd "w") 'srfi-browse-website-url)
    map)
  "Keymap for `srfi-mode'.")

(define-derived-mode srfi-mode special-mode "SRFI"
  "Major mode for browsing Scheme Requests for Implementation.

https://srfi.schemers.org/

\\{srfi-mode-map}"
  (setq-local revert-buffer-function 'srfi-revert)
  (setq-local font-lock-defaults
              '((srfi-mode-font-lock-keywords) nil nil nil nil))
  (unless (equal (buffer-name) "*SRFI*")
    (message (concat "Note: srfi-mode is only meant for the *SRFI* buffer. "
                     "Try M-x srfi."))))

(defun srfi--reverse-iota (count)
  "Internal function to return the integers 0 below COUNT as a list."
  (let ((n 0) (ns '()))
    (while (< n count)
      (push n ns)
      (setq n (+ n 1)))
    ns))

(defun srfi--narrow-to-regexp (regexp)
  "Internal function to narrow the *SRFI* buffer based on REGEXP."
  (save-match-data
    (with-current-buffer (get-buffer "*SRFI*")
      (with-selected-window (get-buffer-window (current-buffer))
        (cl-assert (null (buffer-file-name)))
        (let* ((inhibit-read-only t)
               (case-fold-search t)
               (srfi-count (truncate (length srfi-data) 3))
               (srfi-numbers (if srfi-narrow-keyword
                                 (reverse (cdr (assoc srfi-narrow-keyword
                                                      srfi-data-keywords)))
                                 (srfi--reverse-iota srfi-count))))
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
          (dolist (number srfi-numbers)
            (let* ((base   (* number 3))
                   (year   (elt srfi-data base))
                   (status (elt srfi-data (+ base 1)))
                   (title  (elt srfi-data (+ base 2)))
                   (s-text (cl-case status
                             ((final) year)
                             ((withdrawn) (format "%S, withdrawn" year))
                             (t status)))
                   (line   (format "SRFI %3d: %s (%s)\n"
                                   number title s-text))
                   (beg    (point)))
              (when (or (= 0 (length regexp))
                        (string-match regexp line))
                (insert line)
                (let ((end (point)))
                  (put-text-property beg end 'srfi-number number)))))
          (srfi--goto-first-srfi)
          regexp)))))

(defun srfi--narrow-to-string (string)
  "Internal function to narrow the *SRFI* buffer to STRING."
  (srfi--narrow-to-regexp (regexp-quote string)))

(defun srfi--narrow-to-number (number)
  "Internal function to narrow *SRFI* buffer to full or partial SRFI NUMBER."
  (srfi--narrow-to-regexp
   (if (and (integerp number) (>= number 0))
       (concat "^SRFI +" (regexp-quote (number-to-string number)) "[0-9]*:")
     "")))

(defun srfi--narrow-to-number-or-string (query)
  (let ((number (if (stringp query) (srfi--parse-number query) query)))
    (cond (number
           (srfi--narrow-to-number number)
           number)
          (t
           (srfi--narrow-to-string query)
           query))))

(defun srfi--narrow-minibuffer (&rest _ignored)
  "Internal function to narrow the *SRFI* buffer based on minibuffer."
  (srfi--narrow-to-number-or-string (minibuffer-contents)))

(defun srfi-revert (&optional _arg _noconfirm)
  "(Re-)initialize the *SRFI* buffer."
  (srfi--narrow-to-number-or-string srfi-narrow-query)
  (when (numberp srfi-narrow-query)
    (srfi--goto-number srfi-narrow-query)))

;;;###autoload
(defun srfi-list ()
  "Show the *SRFI* buffer."
  (interactive)
  (let ((old (get-buffer "*SRFI*")))
    (unless (and old (eq old (current-buffer)))
      (switch-to-buffer-other-window (get-buffer-create "*SRFI*")))
    (unless old
      (setq-local default-directory (expand-file-name "~"))))
  (srfi-revert))

;;;###autoload
(defun srfi-search (query)
  "Show the *SRFI* buffer and live-narrow it from the minibuffer.

When called from Emacs Lisp code, QUERY is the string or SRFI
number.  The number can be passed as an integer or a string."
  (interactive
   (minibuffer-with-setup-hook
       (lambda () (add-hook 'after-change-functions
                            #'srfi--narrow-minibuffer
                            nil 'local))
     (srfi-list)
     (list (read-string "SRFI: " srfi-narrow-query))))
  (setq srfi-narrow-query (or (srfi--parse-number query) query))
  (srfi-list))

(defun srfi-fresh-search ()
  "Show the *SRFI* buffer and live-narrow it from scratch."
  (interactive)
  (setq srfi-narrow-query "")
  (call-interactively #'srfi-search))

(defun srfi-keyword (keyword)
  "Show the *SRFI* buffer and narrow it to a paricular KEYWORD."
  (interactive (list (completing-read
                      "Narrow SRFIs to keyword: " srfi-data-keywords
                      nil t nil nil (list nil))))
  (setq srfi-narrow-keyword keyword)
  (unless (= 0 (length srfi-narrow-query))
    (message "NOTE: The SRFI search filter is still active."))
  (srfi-list))

(defun srfi-dired ()
  "Open directory containing SRFI document in Dired."
  (interactive)
  (unless srfi-source-directory
    (error "You must set `srfi-source-directory' first"))
  (dired
   (substitute-in-file-name
    (format "%s/srfi-%d/" srfi-source-directory (srfi--number-on-line)))))

(defun srfi-source ()
  "Open SRFI document."
  (interactive)
  (unless srfi-source-directory
    (error "You must set `srfi-source-directory' first"))
  (let ((srfi (srfi--number-on-line)))
    (find-file
     (substitute-in-file-name
      (format "%s/srfi-%d/srfi-%d.html" srfi-source-directory srfi srfi)))))

(defun srfi-abstract ()
  "Open SRFI abstract document."
  (interactive)
  (unless srfi-abstract-directory
    (error "You must set `srfi-abstract-directory' first"))
  (find-file
   (substitute-in-file-name
    (format "%s/%d.html" srfi-abstract-directory (srfi--number-on-line)))))

;;;###autoload
(defalias 'srfi 'srfi-search)

(provide 'srfi)

;;; srfi.el ends here
