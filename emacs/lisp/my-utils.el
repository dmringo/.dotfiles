;;; Stolen from https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; This is a likely fragile function used for a pandoc-mode @@-directive to
;; include lines from a file.
(defun my/pandoc-include-lines (_ &optional spec)
  (let* ((split    (split-string spec ":"))
         (filename (car split))
         (start    (cadr split))
         (end      (caddr split))
         (cmd      (format
                    "sed -ne '%s,%sp' %s"
                    start end filename)))
    (shell-command-to-string cmd)
    ))

(provide 'my-utils)
