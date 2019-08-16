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


;; ** Key bindings
(global-set-key (kbd "M-Q") #'my/unfill-paragraph)
(global-set-key (kbd "C-x C-o") #'my/other-win)


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


;; make scrolling less jarring
(setq
 ;; This makes scrolls only move a given fraction of the window at a time
 scroll-up-aggressively    0.1
 scroll-down-aggressively  0.1
 ;; This sets the margin at which scrolling will happen when the point enters it
 scroll-margin             10)

;; Enable some commands that I use. Normally these are disabled and
;; will generate a warning on use
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'upcase-region 'disabled nil); C-x C-u
(put 'narrow-to-region 'disabled nil) ; C-x n n

;; Don't like the startup screen
(setq inhibit-startup-screen t)

;; Backup/autosave policies
(setq
 ;; Put backups matching first CAR regex at CDR. I like them all in
 ;; one place.
 backup-directory-alist          `((".*" . ,(user-emacs-file "backups")))
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
 auto-save-file-name-transforms  '((".*" "~/.emacs.d/auto-save/" t)))


;; Keep things changed by Customize in a separate file.  It really
;; shouldn't be much these days
(defconst my-custom-file (user-emacs-file "lisp/my-custom.el"))
(setq custom-file my-custom-file)
;; Load the custom file
(when (file-readable-p custom-file) (load custom-file))


(provide 'my-simple)
