;;; srfi.el --- Scheme Requests for Implementation
;;
;; SPDX-FileCopyrightText: 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-srfi
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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

(defun srfi-list ()
  (interactive)
  (with-displayed-buffer-window
   "*SRFI*" (list #'display-buffer-pop-up-window) nil
   (with-current-buffer (get-buffer-create "*SRFI*")
     (cl-assert (null (buffer-file-name)))
     (erase-buffer)
     (insert "Scheme Requests for Implementation\n\n")
     (dolist (srfi (reverse srfi-data))
       (cl-destructuring-bind (number status title year) srfi
         (insert (format "SRFI %3d: %s (%s)\n" number title
                         (case status
                           ((final) year)
                           ((withdrawn) (format "%S, withdrawn" year))
                           (t status)))))))))

(defun srfi--number-on-line ()
  (save-match-data
    (save-excursion
      (goto-char (point-at-bol))
      (or (and (looking-at "^SRFI +\\([0-9]+\\):")
               (string-to-number (match-string 1)))
          (error "No SRFI on this line")))))

(defun srfi--version-control-url (srfi-number)
  (format "https://github.com/scheme-requests-for-implementation/srfi-%d/"
          srfi-number))

(defun srfi--discussion-url (srfi-number)
  (format "https://srfi-email.schemers.org/srfi-%d/" srfi-number))

(defun srfi--landing-page-url (srfi-number)
  (format "https://srfi.schemers.org/srfi-%d/"
          srfi-number))

(defun srfi--document-url (srfi-number)
  (format "https://srfi.schemers.org/srfi-%d/srfi-%d.html"
          srfi-number srfi-number))

(defun srfi-browse-version-control-url ()
  (interactive)
  (browse-url (srfi--version-control-url (srfi--number-on-line))))

(defun srfi-browse-discussion-url ()
  (interactive)
  (browse-url (srfi--discussion-url (srfi--number-on-line))))

(defun srfi-browse-landing-page-url ()
  (interactive)
  (browse-url (srfi--landing-page-url (srfi--number-on-line))))

(defun srfi-browse-url ()
  (interactive)
  (browse-url (srfi--document-url (srfi--number-on-line))))

(provide 'srfi)

;;; srfi.el ends here
