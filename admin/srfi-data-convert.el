;; Copyright 2019, 2020 Lassi Kortela, 2025 Arthur A. Gleckler
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

;; TODO: Currently copy-pasted from <srfi-common/srfi-tools/data.scm>.
(defconst srfi-data-convert--keyword-to-title
  '((algorithm "Algorithm")
    (assignment "Assignment")
    (binding "Binding")
    (comparison "Comparison")
    (concurrency "Concurrency")
    (continuations "Continuations")
    (control-flow "Control Flow")
    (data-structure "Data Structure")
    (error-handling "Error Handling")
    (exceptions "Exceptions")
    (features "Features")
    (garbage-collection "Garbage Collection")
    (i/o "I/O")
    (internationalization "Internationalization")
    (introspection "Introspection")
    (lazy-evaluation "Lazy Evaluation")
    (miscellaneous "Miscellaneous")
    (modules "Modules")
    (multiple-value-returns "Multiple-Value Returns")
    (numbers "Numbers")
    (object-oriented "Object-Oriented Programming")
    (operating-system "Operating System")
    (optimization "Optimization")
    (parameters "Parameters")
    (pattern-matching "Pattern Matching")
    (record-type "Record Type")
    (r6rs-process "R6RS process")
    (r7rs-large "R7RS Large")
    (r7rs-large-red "R7RS Large: Red Edition")
    (r7rs-large-tangerine "R7RS Large: Tangerine Edition")
    (randomness "Randomness")
    (reader-syntax "Reader Syntax")
    (sicp "SICP")
    (superseded "Superseded")
    (syntax "Syntax")
    (testing "Testing")
    (text "Text")
    (type-checking "Type Checking")))

(defun srfi-data-convert--read-all ()
  (with-temp-buffer
    (insert-file-contents "srfi-data.scm")
    (let ((forms '()))
      (condition-case nil
          (while t (setq forms (nconc forms (list (read (current-buffer))))))
        (end-of-file forms)))))

(defun srfi-data-convert--assert-valid-numbering (srfis)
  (let ((next-number 0))
    (mapc (lambda (srfi)
            (let ((number (cadr (assoc 'number srfi))))
              (unless (equal number next-number) (error "Bad numbering"))
              (setq next-number (1+ next-number))))
          srfis)))

(defun srfi-data-convert--write-all (srfis)
  (srfi-data-convert--assert-valid-numbering srfis)
  (let ((by-keyword
         (mapcar #'list (sort (mapcar #'cadr
                                      srfi-data-convert--keyword-to-title)
                              #'string<))))
    (with-temp-buffer
      (insert
       ";;; srfi-data.el --- Scheme Requests for Implementation database"
       " -*- lexical-binding: t -*-" "\n"
       ";;" "\n"
       ";; Copyright 1998-2020 srfi.schemers.org" "\n"
       ";; SPDX-License-Identifier: MIT" "\n"
       ";;" "\n"
       ";;; Commentary:" "\n"
       ";;" "\n"
       ";; Automatically converted from srfi-data.scm." "\n"
       ";;" "\n"
       ";;; Code:" "\n"
       "\n"
       "(defconst srfi-data" "\n"
       "  [\n")
      (mapc (lambda (srfi)
              (let* ((number (cadr (assoc 'number srfi)))
                     (status (cadr (assoc 'status srfi)))
                     (title  (cadr (assoc 'title  srfi)))
                     (keywords (cdr (assoc 'keywords srfi)))
                     (year   (and (not (equal 'draft status))
                                  (let ((date (cadr (assoc 'done-date srfi))))
                                    (string-to-number
                                     (substring date 0 4))))))
                (mapc (lambda (keyword)
                        (let* ((keyword
                                (cadr (assoc
                                       keyword
                                       srfi-data-convert--keyword-to-title)))
                               (apair (assoc keyword by-keyword)))
                          (setcdr apair (append (cdr apair) (list number)))))
                      keywords)
                (insert (format "   ;; SRFI %d:\n" number)
                        (format "   %S %S %S\n" year status title))))
            srfis)
      (insert "   ]" "\n"
              "  \"Table of all known SRFI documents.\")" "\n"
              "\n")
      (insert "(defconst srfi-data-keywords" "\n"
              "  '(" "\n")
      (mapc (lambda (apair)
              (let ((title (car apair))
                    (numbers (cdr apair)))
                (insert (format "    (%S\n" title))
                (while numbers
                  (let ((n (min 16 (length numbers))))
                    (insert "     "
                            (mapconcat #'prin1-to-string
                                       (cl-subseq numbers 0 n)
                                       " "))
                    (setq numbers (nthcdr n numbers))
                    (when numbers (insert "\n"))))
                (insert ")\n")))
            by-keyword)
      (insert "    )" "\n"
              "  \"Table of SRFI numbers by keyword.\")" "\n"
              "\n"
              "(provide 'srfi-data)" "\n"
              "\n"
              ";;; srfi-data.el ends here" "\n")
      (write-region nil nil "srfi-data.el.new"))))

(defun srfi-data-convert ()
  (interactive)
  (srfi-data-convert--write-all (srfi-data-convert--read-all)))
