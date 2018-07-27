;;; -*- lexical-binding: t -*-

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
         (re-spec        (concat "^" re-filename ":" re-tag "$"))
         (good-spec      (string-match re-spec spec))
         (filename       (match-string 1 spec))
         (tag            (match-string 2 spec))
         (found-any      nil)
         (results        nil)
         (re-tag-in-file (concat "^" (regexp-quote (or tag "")) " *$")))
    (if good-spec
        (with-temp-buffer
          (insert-file-contents filename)
          (goto-char 1)
          (while (re-search-forward re-tag-in-file nil t)
            (setq found-any t)
            (let ((beg (+ 1 (point)))
                  (end (re-search-forward re-tag-in-file nil t)))
              (if (and beg end)
                  (push (buffer-substring beg (line-beginning-position))
                        results)
                (error "Unmatched occurrences of \"%s\" in %s"
                       tag filename))))
          (if found-any
              (string-join (reverse results))
            (error "No tag found matching \"%s\" in %s"
                   tag filename)))
      (error (format "bad tag spec: \"%s\"" spec)))))

(defun my/insert-date (&optional timespec)
  (interactive)
  (let ((spec (or timespec  "%b %e, %Y")))
    (insert (format-time-string spec))))


(defun my/ansi-term-zsh ()
  (interactive)
  (ansi-term "zsh"))

;; Stolen from Emacs Prelude (https://github.com/bbatsov/emacs-prelude)
;;
;; TODO: If prefix arg is specified and in a recognized project, copy name
;; relative to project root
(defun my/clip-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; Handler for 'tel:' links via Google Voice
(require 'org)
(defun my/org-gvoice-dial (number)
  "Set up a call to NUMBER via the Google Voice website.
This still requires some interaction, signing in to Google if
necessary and selecting a number to call from."
  (let ((url
         (format "https://voice.google.com/u/0/calls?a=nc,%s"
                 number)))
    (message "Calling %s via %s" number url)
    (browse-url url)))

(org-add-link-type "tel" 'my/org-gvoice-dial)

(provide 'my-utils)
