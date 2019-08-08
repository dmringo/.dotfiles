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

(defun my/blame-me (&optional timespec)
  (interactive)
  (let* ((time (format-time-string (or timespec  "%b %e, %Y")))
         (str (format "[%s - %s]" (user-login-name) time)))
    (insert str)))

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


(defun my/fill-line (char)
  "Fill in a line with a repeated character.
The end of the fill is determined by (`or' `fill-column'
`comment-fill-column').  If both are falsey or the `point' is
past the end, no action is taken."
  (interactive "cCharacter to fill:")
  (when-let* ((end (or fill-column comment-fill-column))
              (_ (integerp end))
              (n (- end (current-column)))
              (_ (< 0 n)))
    (insert (make-string n char))))


(defun my/relativize-path-at-point ()
  "Read a path at the point and convert it to a relative path"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'filename))
         (beg (car bounds))
         (end (cdr bounds))
         (path (buffer-substring-no-properties beg end)))
    (when (file-name-absolute-p path)
      (delete-region beg end)
      (insert (file-relative-name path)))))

(defun my/frame-font-resize (delta)
  "Resize the current frame's font size by DELTA points.
DELTA is taken as the numeric prefix arg or from the prompt if no
prefix arg is given. This changes the attributes (removing some)
on the underlying font object, but seems to preserve the
important ones."
  (interactive "NResize font by how much? ")
  (let* ((font-keys '(:family :weight :slant :width :size))
         (attr-pairs
          (mapcar (lambda (k)
                    (let ((attr (font-get (face-attribute 'default :font) k)))
                      (when (eq k :size) ; adjust only the size
                        (setq attr (+ attr delta)))
                      (list k attr))) font-keys))
         (font (apply #'font-spec (apply #'append attr-pairs))))
    (set-face-attribute 'default nil :font font)))


(defun my/other-win (arg)
  "Like `other-window' but with a conveneient transient map.

Similar to `text-scale-adjust', after initial invocation, <o> and
<C-o> both repeat `other-window', moving focus to the next window
in the cyle, whereas <O> and <C-S-o> move backwards, as if
`other-window' was called with an argument of negative 1."
  (interactive "p")
  (other-window arg)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "C-o") (lambda () (interactive) (my/other-win 1)))
     (define-key map (kbd "o")   (lambda () (interactive) (my/other-win 1)))
     (define-key map (kbd "O")   (lambda () (interactive) (my/other-win -1)))
     (define-key map (kbd "C-S-o") (lambda () (interactive) (my/other-win -1)))
     map)))

(defmacro my/log-var* (&rest FORMS)
  "Log the values and literal forms of FORMS to buffer *my/log*
Useful for printf-style debugging.  Probably buggy itself though..."
  `(with-current-buffer
       (get-buffer-create "*my/log*")
     (save-excursion
       (goto-char (point-max))
       ,@(mapcar
          (lambda (sym)
            `(insert
              (format "%s:\n%s\n\n"
                      ',sym (pp-to-string ,sym))))
          syms))))


(defun my/ssh-agent-refresh ()
  "set ssh-agent values in the environment"
  (interactive)
  (with-temp-buffer
    (call-process "ssh-agent" nil t)
    (goto-char (point-min))
    (re-search-forward "^SSH_AUTH_SOCK=\\([^;]+\\);")
    (when-let ((sock (match-string 1)))
      (message "setting SSH_AUTH_SOCK: %s" sock)
      (setenv "SSH_AUTH_SOCK" sock))
    (re-search-forward "^SSH_AGENT_PID=\\([0-9]+\\);")
    (when-let ((pid (match-string 1)))
      (message "setting SSH_AGENT_PID: %s" pid)
      (setenv "SSH_AGENT_PID" pid))))


(provide 'my-utils)
