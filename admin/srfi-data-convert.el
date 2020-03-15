(defun srfi-data-convert--read-all ()
  (with-temp-buffer
    (insert-file-contents "srfi-data.scm")
    (let ((forms '()))
      (condition-case nil
          (while t (setq forms (nconc forms (list (read (current-buffer))))))
        (end-of-file forms)))))

(defun srfi-data-convert--write-all (srfis)
  (with-temp-buffer
    (insert
     ";;; srfi-data.el --- Scheme Requests for Implementation database"
     " -*- lexical-binding: t -*-" "\n"
     ";;" "\n"
     ";;; Commentary:" "\n"
     ";;" "\n"
     ";; Automatically converted from srfi-data.scm." "\n"
     ";;" "\n"
     ";;; Code:" "\n"
     "\n"
     "(defconst srfi-data" "\n"
     "  [\n")
    (let ((next-number 0))
      (mapc (lambda (srfi)
              (let* ((number (cadr (assoc 'number srfi)))
                     (status (cadr (assoc 'status srfi)))
                     (title  (cadr (assoc 'title  srfi)))
                     (year   (and (not (equal 'draft status))
                                  (let ((date (cadr (assoc 'done-date srfi))))
                                    (string-to-number
                                     (substring date 0 4))))))
                (unless (equal number next-number) (error "Bad numbering"))
                (setq next-number (1+ next-number))
                (insert (format "   ;; SRFI %d\n" number)
                        (format "   %S %S %S\n" year status title))))
            srfis))
    (insert "   ]" "\n"
            "  \"Table of all known SRFI documents.\")" "\n"
            "\n"
            "(provide 'srfi-data)" "\n"
            "\n"
            ";;; srfi-data.el ends here" "\n")
    (write-region nil nil "srfi-data.el.new")))

(defun srfi-data-convert ()
  (interactive)
  (srfi-data-convert--write-all (srfi-data-convert--read-all)))
