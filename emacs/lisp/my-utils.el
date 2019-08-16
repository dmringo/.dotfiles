;;; -*- lexical-binding: t -*-

;; TODO: move this closer to Pandoc config
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
  "Insert current date with TIMESPEC or \"%F\" if TIMESPEC is
nil"
  (interactive)
  (let ((spec (or timespec  "%F")))
    (insert (format-time-string spec))))


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
                      (list k attr)))
		  font-keys))
         (font (apply #'font-spec (apply #'append attr-pairs))))
    (set-face-attribute 'default nil :font font)))


(defun my/ssh-agent-refresh ()
  "Create a new ssh-agent and set the appropriate variables."
  ;; TODO: try to use the same stuff from .profile?
  ;;
  ;; In theory, the .profile logic should ensure the environment is already
  ;; properly set up for emacs
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
