;;; Stolen from https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my/pandoc-include-lines (_ spec)
  "Meant to be used as an @@-directive in pandoc-mode. The string
argument SPEC should be of the form \"FILENAME:START[(+|-)END]\".
This eventually shells out to 'sed', so the FILENAME shoud be
either a fully-qualified path or a path relative to the directory
in which the pandoc input file lives. START and END should be
numeric.  START always refers to the first line to include from
the given file. If no endspec (\"+END\" or \"-END\") is given,
only the first line is included.  If the endspec is of form
\"\"\"+END\", END is taken as the total number of lines to
include, starting from START.  If it takes the form \"-END\", END
is taken as the last line to include.

Examples:

@@lines{foo.txt:13} - Include only line 13 from foo.txt
@@lines{foo.txt:13+4} - Include lines 13 to 16, inclusive
@@lines{foo.txt:13-20} - Include lines 13 to 20, inclusive

This assumes that there is a pair in the variable `pandoc-directives'
(\"lines\" . my/pandoc-include-lines)."
  (let*
      ((safe-read (lambda (str)
                    (when (stringp str)
                      (car (read-from-string str)))))
       (re-digits "\\([[:digit:]]+\\)")
       (re-dash-or-plus "\\([+-]\\)")
       (re-filename "\\(.+\\)")
       (regex
         (concat "^" ; want to match the whole spec string
                 re-filename
                 ":" ; colon separator
                 re-digits ; Always need a start line
                 "\\(" re-dash-or-plus re-digits "\\)?" ; end spec is optional
                 "$" ; again, must match the whole string
                 ))
       (match (string-match regex spec))
       (filename (match-string 1 spec))
       (start (funcall safe-read (match-string 2 spec)))
       (operator (match-string 4 spec))
       (endspec (funcall safe-read (match-string 5 spec)))
       (end (cond ((string= operator "+")
                   (+ start endspec (- 1)))
                  ((string= operator "-")
                   endspec)
                  (t start))))
    (if match
        (shell-command-to-string
         (format "sed -ne '%d,%dp' %s" start end filename))
      (error (format "bad lines spec: \"%s\"" spec)))))

(defun my/pandoc-include-tag (_ spec)
  (let* ((re-filename    "\\(.+?\\)")
         (re-tag         "\\(.+\\)")
         (regex          (concat "^" re-filename ":" re-tag "$"))
         (match          (string-match regex spec))
         (filename       (match-string 1 spec))
         (tag            (match-string 2 spec))
         (re-tag-in-file (concat "^" (regexp-quote (or tag "")))))
    (if match
        (with-temp-buffer
          (insert-file-contents filename)
          (goto-char 1)
          (let ((beg (re-search-forward re-tag-in-file nil t))
                (end (re-search-forward re-tag-in-file nil t)))
            (if (and beg end)
                (progn
                  (goto-char beg)
                  (forward-line 1)
                  (push-mark)
                  (goto-char end)
                  (beginning-of-line)
                  (buffer-substring (region-beginning) (region-end))
                  )
              (error (format
                      "Couldn't find two occurrences of \"%s\" in %s"
                      tag filename))
              )))
      (error (format "bad tag spec: \"%s\"" spec)))))

(provide 'my-utils)
