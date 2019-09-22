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
    (insert ";;; Automatically converted from srfi-data.scm.\n")
    (mapc (lambda (form) (insert (format "%S\n" form)))
          `((defconst srfi-data ',srfis
                      "Table of all known SRFI documents.")
            (provide 'srfi-data)))
    (write-region nil nil "srfi-data.el")))

(defun srfi-data-convert ()
  (interactive)
  (srfi-data-convert--write-all
   (srfi-data-convert--convert
    (srfi-data-convert--read-all))))
