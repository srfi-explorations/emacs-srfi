(defun srfi-data-convert--read-all ()
  (with-temp-buffer
    (insert-file-contents "srfi-data.scm")
    (let ((forms '()))
      (condition-case nil
          (while t (setq forms (nconc forms (list (read (current-buffer))))))
        (end-of-file forms)))))

(defun srfi-data-convert--convert (original-srfis)
  (mapcar (lambda (srfi)
            (let* ((number (cadr (assoc 'number srfi)))
                   (status (cadr (assoc 'status srfi)))
                   (title  (cadr (assoc 'title  srfi)))
                   (year   (and (not (equal 'draft status))
                                (string-to-number
                                 (substring (cadr (assoc 'done-date srfi))
                                            0 4)))))
              (list number status title year)))
          original-srfis))

(defun srfi-data-convert--write-all (srfis)
  (with-temp-buffer
    (insert ";;; Automatically converted from srfi-data.scm.\n\n")
    (insert "(defconst srfi-data\n")
    (insert "  '(\n")
    (mapc (lambda (srfi) (insert (format "    %S\n" srfi)))
          srfis)
    (insert "    )\n")
    (insert "  \"Table of all known SRFI documents.\")\n\n")
    (insert "(provide 'srfi-data)\n")
    (write-region nil nil "srfi-data.el")))

(defun srfi-data-convert ()
  (interactive)
  (srfi-data-convert--write-all
   (srfi-data-convert--convert
    (srfi-data-convert--read-all))))
