;; -*- lexical-binding: t -*-
;;
;; This file is meant for customization of basic features in Emacs
;; that apply across many modes or globally as well as some simple
;; utility functions.  Ideally, it can be used in an emacs -q session
;; if I've broken some other part of my config


;; ** Defuns

;; Stolen from https://www.emacswiki.org/emacs/UnfillParagraph
;; The opposite of fill-paragraph.
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Stolen from Emacs Prelude (https://github.com/bbatsov/emacs-prelude)
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

(require 'pp)
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

(defvar my/emacs-init-file
  (file-truename (locate-user-emacs-file "init.el"))
  "Real path to my init.el file")

(defun my/visit-emacs-init-file ()
  "Visit `my/emacs-init-file' or pop to the buffer already
visiting it"
  (interactive)
  (or (pop-to-buffer
       (get-file-buffer my/emacs-init-file))
      (find-file my/emacs-init-file)))


(defvar my/kill-with-query-bufs
  '("*scratch*")
  "List of buffer names that merit an additional query before being killed")

(add-to-list
 'kill-buffer-query-functions
 (defun my/kill-buf-query ()
   "Function to check if a buffer should *really* be killed.
Only buffers with names matching one in `my/kill-with-query-bufs'
cause an interactive query, all others are ignored. Membership is
tested with `member'"
   (let ((cb (current-buffer)))
     (or (not (member (buffer-name cb)
                      my/kill-with-query-bufs))
         (yes-or-no-p
          (format
           ;; elisp repr is fine here - makes it extra clear we're talking about
           ;; a buffer
           "Do you really want to kill %S? " cb))))))



;; ** Key bindings

(let ((spec nil)
      (map (current-global-map))
      (bindings
       `(("M-Q"     . ,#'my/unfill-paragraph)
         ("C-x C-o" . ,#'my/other-win)
         ("C-M-}"   . ,#'enlarge-window-horizontally)
         ("C-M-{"   . ,#'shrink-window-horizontally)
         ;; Slightly quicker Kill this buffer
         ("C-x k"   . ,#'kill-this-buffer)
         ;; Disable `xref' stuff in global map and put it in prog-mode-map
         ;; instead
         ("M-.") ;; nil
         ("M-,") ;; nil

         :map ,prog-mode-map
         ("M-."     . ,#'xref-find-definitions)
         ("M-,"     . ,#'xref-pop-marker-stack)

         :map ,help-map
         ("M"       . ,#'man)
         ("W"       . ,#'woman)
         ;; Not really a "help" function, but it's similarly introspective
         ("E"       . ,#'my/visit-emacs-init-file))))
  (while bindings
    (setq spec (pop bindings))
    (pcase spec
      (:map
       (setq map (pop bindings)))
      (`(,key . ,def)
       (define-key map (kbd key) def)))))


;; ** Misc Customizations

;; Bars suck
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  ;; Don't really like big fringes much either
  (set-fringe-mode '(0 . 0)))

;; Be Lazy, prefer Y or N to Yes or No
(fset 'yes-or-no-p 'y-or-n-p)

;; Make paste over selection *replace* the selection
(delete-selection-mode)

;; Column numbers are good
(column-number-mode)

;; use View-mode in read-only buffers
;; TODO: use view mode for only some read-only buffers using
;; `read-only-mode-hook' (setq view-read-only t)
(with-eval-after-load 'view
  (define-key view-mode-map (kbd "j") #'scroll-up-line)
  (define-key view-mode-map (kbd "k") #'scroll-down-line))


;; When narrowing to a [de]fun[ction], include preceding comments
(setq narrow-to-defun-include-comments t)

;; Always move to help window after opening (easier to close)
(setq help-window-select t)

;; Prefer horizontal (i.e. [L] | [R]) splits, without making stupidly narrow
;; windows
(setq split-height-threshold nil
      split-width-threshold 120)

;; Always include some path context in buffer names
(setq uniquify-min-dir-content 3)

;; make Dired buffers have "Dired: " prefix for easier filtering while switching
;; buffers
(add-hook
 'dired-after-readin-hook
 (defun my/rename-dired-buffer ()
   (let ((bname (buffer-name (current-buffer))))
     (unless (string-match "^Dired: " bname)
       (rename-buffer (concat "Dired: " bname))))))

;; Don't prompt when reverting PDFs
(add-to-list 'revert-without-query ".*\\.pdf")

;; Do not ping known domains when finding file at point
(setq ffap-machine-p-known "reject")

;; Don't ask about following links to source-controlled files -- just do it
(setq vc-follow-symlinks t)

;; Always prefer the newer version of a file.
(setq load-prefer-newer t)

;; Basic indentation rules
(setq-default indent-tabs-mode nil
              tab-width 2)

;; use 80 cols as a reasonable line length limit
(setq-default fill-column 80)

;; I like boxes
(setq cursor-type 'box)

;; use text-mode in new buffers by default
(setq-default major-mode 'text-mode)

;; Enable recursive minibuffers (useful e.g. when using something like
;; `counsel-yank-pop' to insert something from far back in the kill ring at a
;; minibuffer prompt)
(setq enable-recursive-minibuffers t)


;; make scrolling less jarring
(setq-default
 ;; This makes scrolls only move a given fraction of the window at a time.  0.0
 ;; specifically means scroll just one line.
 scroll-up-aggressively    0.0
 scroll-down-aggressively  0.0
 ;; This sets the margin at which scrolling will happen when the point enters it
 scroll-margin             10)

;; Enable some commands that I use. Normally these are disabled and
;; will generate a warning on use
(put 'downcase-region           'disabled nil) ;; "C-x C-l" in global map
(put 'upcase-region             'disabled nil) ;; "C-x C-u" in global map
(put 'narrow-to-region          'disabled nil) ;; "C-x n n" in global map
(put 'dired-find-alternate-file 'disabled nil) ;; "a" in dired map



;; Don't like the startup screen
(setq inhibit-startup-screen t)

;; Backup/autosave policies
(setq
 ;; Put backups matching first CAR regex at CDR. I like them all in
 ;; one place.
 backup-directory-alist          `((".*" . ,(locate-user-emacs-file "backups")))
 ;; Anything non-nil and non-t means don't do any deletion of old
 ;; versions.
 delete-old-versions             -1
 ;; t means add version numbers to backup files
 version-control                 t
 ;; Even backup files under some VC system (because I don't commit
 ;; often enough)
 vc-make-backup-files            t
 ;; Put all autosave files in the same place (similar to
 ;; `backup-directory-alist'.  Third list element means that auto-save
 ;; should uniquify the file name using the directory part of the
 ;; filename
 auto-save-file-name-transforms  `((".*" ,(locate-user-emacs-file "auto-save") t)))

;; simulate the emacs source tree .dir-locals file for any elisp under the
;; default emacs install root
;; NOTE: have to set class variable *before* setting directory class
(dir-locals-set-class-variables
 'emacs
 '((nil . ((tab-width . 8)
           (sentence-end-double-space . t)
           (fill-column . 70)
           (bug-reference-url-format . "https://debbugs.gnu.org/%s")))
   (emacs-lisp-mode . ((indent-tabs-mode . nil)
                       (electric-quote-comment . nil)
                       (electric-quote-string . nil)
	                     (mode . bug-reference-prog)))
   (outline-mode . ((mode . bug-reference)))))
;; TODO: use `data-directory' to get this dynamically?
(dir-locals-set-directory-class "/usr/local/share/emacs" 'emacs)



;; Keep things changed by Customize in a separate file.  It really
;; shouldn't be much these days
(defconst my-custom-file (locate-user-emacs-file "lisp/my-custom.el"))
(setq custom-file my-custom-file)
;; Load the custom file
(when (file-readable-p custom-file) (load custom-file))


(provide 'my-simple)
