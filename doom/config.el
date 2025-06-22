;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(defun my/obfuscate (str)
  "obfuscate STR as a string appropriate for inclusion in a public"
  (interactive "Mstring to obfuscate:")
  (funcall
   (if (interactive-p)
       #'message
     #'identity)
   (concat
    "\""
    (apply #'concat
           (mapcar
            (lambda (c)
              (format "\\x%x" c))
            (rot13 str)))
    "\"")))
(fset 'my/deobfuscate #'rot13-string)


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "David Ringo"
       user-mail-address
       ;; overly complicated obfuscation for bots
       (my/deobfuscate
        (concat
         "\x71\x6e\x69\x76\x71\x7a\x65"
         "\x76\x61\x74\x62\x40\x74\x7a"
         "\x6e\x76\x79\x2e\x70\x62\x7a")))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq! org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Good delay while I'm getting used to doom's keybindings
(setq which-key-idle-delay 0.2)

;; TODO: Find proper way to make "vc" the default action for switch proj
(after! (:and counsel-projectile projectile)
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((move counsel-projectile-switch-project-action-vc 1)
     (setkey counsel-projectile-switch-project-action-vc "o")
     (setkey counsel-projectile-switch-project-action " ")))
  (setq! projectile-switch-project-action 'projectile-vc))


(setq! c-basic-offset 2)

(after! cc-mode
  (fset 'c-indent-region 'clang-format-region)
  (fset 'c-indent-line-or-region 'clang-format))


(setq! haskell-process-type 'stack-ghci)


;; Better buffer handing for {async-,}shell-command
(advice-add
 'shell-command
 :after
 (defun shell-rename-buffer (cmd &optional o-buf _)
   "Preserve shell-command output in a well-named buffer unless
one is manually specified."
   (unless o-buf
     (let* ((async? (string-match "[ \t]*&[ \t]*\\'" cmd))
            (new-bufname ;; what we'll call the new buffer
             (format "*%s shell: %s*" (if async? "async" "sync") cmd))
            (orig-bufname ;; what the old buffer was calld
             (if async? "*Async Shell Command*" "*Shell Command Output*")))
       (with-current-buffer orig-bufname
         (rename-buffer new-bufname t))))))

(defun my/unfill-paragraph (&optional region)
  "Convert a multi-line REGION to a single line of text.
Should be roughly the inverse of `fill-paragraph'"
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(map!
 ;; I like to kill buffers indiscriminately, without delay
 "C-x k" #'kill-current-buffer
 ;; Don't really like the default isearch
 "C-s"   #'swiper-isearch
 ;; Bind `my/unfill-paragraph' similar to `fill-paragraph'
 "M-Q" #'my/unfill-paragraph
 )
