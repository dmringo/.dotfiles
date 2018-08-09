;; -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 100 1024 1024))

(package-initialize)

;; Good to have some secrets
(let ((s-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p s-file)
    (load s-file)))

(defconst my/lisp-dir
  (expand-file-name "lisp" user-emacs-directory))

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

(package-refresh-contents)
(mapc (lambda (pkg)
	      (unless (package-installed-p pkg)
	        (package-install pkg)))
      '(use-package diminish bind-key))

(setq use-package-verbose       t
      use-package-always-ensure t)

;; suggested by jwiegly
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish (I do)
(require 'bind-key) ;; if you use any :bind variant (I do)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; Do this early, in case other packages want something in my $PATH at
;; load/install time
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :ensure org-plus-contrib
  :bind ((:map org-mode-map
               ("<C-tab>" . other-window)))
  :config
  ;; ditaa path as installed by apt
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
        ;; latexmk is a little more consistent than pdflatex
        org-latex-pdf-process (list "latexmk -f -pdf %f")))

(use-package ox-twbs)
(use-package ox-gfm)
(use-package ox-pandoc)
(use-package htmlize)


(defun my/org-babel-load-langs ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ditaa . t)
     (emacs-lisp  . t))))

(add-hook 'after-init-hook 'my/org-babel-load-langs)

;; Undo-tree is great - enable it globally and remove it from the modeline
;; (since it should always be active)
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package magit
  :bind (("M-G" . magit-status)
         :map magit-mode-map
         ("C-<tab>" . nil)
         ("<tab>" . magit-section-cycle)))


(use-package gitignore-mode)
(use-package intero)
(use-package haskell-mode
  :config (add-hook 'haskell-mode-hook 'intero-mode))
(use-package company-quickhelp)
(use-package company 
  :config
  (defun my/company-next () (interactive) (company-complete-common-or-cycle 1))
  (defun my/company-prev () (interactive) (company-complete-common-or-cycle -1))
  :bind (:map company-active-map
              ("M-C-i" . company-complete)
              ("C-c h" . company-quickhelp-manual-begin)
              ("C-n" . my/company-next)
              ("C-p" . my/company-prev)))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))


(use-package smart-mode-line)
(use-package smartparens
  :config (progn
	    (add-hook 'prog-mode-hook 'smartparens-mode)
	    (sp-with-modes 'markdown-mode
			   (sp-local-pair "```" "```")
			   (sp-local-pair "*" "*")
			   (sp-local-pair "_" "_")
			   (sp-local-pair "$" "$"))
	    (sp-with-modes 'c++-mode
			   (sp-local-pair "/*" "*/")))
  :bind (:map smartparens-mode-map
              ("C-c u w" . sp-unwrap-sexp)
              ("C-M-f"   . sp-forward-sexp)
              ("C-M-b"   . sp-backward-sexp)
              ("C-M-k"   . sp-kill-sexp)))

;; Whitespace-related
(use-package whitespace-cleanup-mode
  :diminish
  :config (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package rainbow-delimiters)

;; Show me where the cursor is, when it changes
(use-package beacon
  :init
  (beacon-mode 1)
  (setq beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-point-moves-vertically nil
        beacon-blink-when-window-scrolls 0
        beacon-color 1))

(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config 
  (pdf-tools-install))

(use-package google-this
  :bind (("C-c g" . google-this-mode-submap)))

(use-package zeal-at-point
  :bind (("C-c z" . zeal-at-point)))

;; C++ stuff
(use-package lsp-mode)
(use-package company-lsp
  ;; snippets don't seem to work too well.  At least, I can't figure out how to
  ;; expand them properly
  :config (setq company-lsp-enable-snippet nil))

(use-package lsp-ui)
(use-package cquery
  :commands lsp-cquery-enable
  :config
  (setq cquery-executable (expand-file-name "cquery" "~/.local/bin/")))

(defun my/maybe-enable-cquery ()
  (interactive)
  (when (locate-dominating-file default-directory "compile_commands.json")
    (condition-case nil
        (progn
          (message "enabling cquery")
          (lsp-cquery-enable))
      (user-error nil))))

(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-common-hook #'my/maybe-enable-cquery)

(use-package cmake-mode)

(use-package comment-dwim-2
  :config (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(use-package js2-mode
  :config
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(use-package rjsx-mode
  :diminish
  :init
  (add-to-list 'auto-mode-alist '("[Cc]omponents\\/.*\\.js\\'" . rjsx-mode)))
(use-package json-mode :diminish)

(use-package fill-column-indicator)
(use-package rainbow-mode)

(use-package math-symbol-lists
  :config
  (quail-define-package "math" "UTF-8" "Ω" t)
  (quail-define-rules ; add whatever extra rules you want to define here...
   ("->"      "→") ("<=>"     "↔") ("\\iff"   "↔") ("\\from"  "←")
   ("\\to"    "→") ("\\lhd"   "⊲") ("\\rhd"   "⊳") ("\\unlhd" "⊴")
   ("\\unrhd" "⊵") ("\\Land"  "⋀") ("\\Lor"   "⋁") ("\\ltE"   "Ɛ") 
   ("\\lthkD" "Ɗ") ("\\lthkP" "Ƥ") ("\\lthkV" "Ʋ") ("\\lthkB" "Ɓ") 
   ("\\lthkK" "Ƙ") ("\\hdots" "…") ("^("      "⁽") ("^)"      "⁾")
   ("^+"      "⁺") ("^-"      "⁻") ("^0"      "⁰") ("^1"      "¹")
   ("^2"      "²") ("^3"      "³") ("^4"      "⁴") ("^5"      "⁵")
   ("^6"      "⁶") ("^7"      "⁷") ("^8"      "⁸") ("^9"      "⁹")
   ("^="      "⁼") ("^A"      "ᴬ") ("^B"      "ᴮ") ("^D"      "ᴰ")
   ("^E"      "ᴱ") ("^G"      "ᴳ") ("^H"      "ᴴ") ("^I"      "ᴵ")
   ("^J"      "ᴶ") ("^K"      "ᴷ") ("^L"      "ᴸ") ("^M"      "ᴹ")
   ("^N"      "ᴺ") ("^O"      "ᴼ") ("^P"      "ᴾ") ("^R"      "ᴿ")
   ("^T"      "ᵀ") ("^U"      "ᵁ") ("^V"      "ⱽ") ("^W"      "ᵂ")
   ("^a"      "ᵃ") ("^b"      "ᵇ") ("^c"      "ᶜ") ("^d"      "ᵈ")
   ("^e"      "ᵉ") ("^f"      "ᶠ") ("^g"      "ᵍ") ("^h"      "ʰ")
   ("^i"      "ⁱ") ("^j"      "ʲ") ("^k"      "ᵏ") ("^l"      "ˡ")
   ("^m"      "ᵐ") ("^n"      "ⁿ") ("^o"      "ᵒ") ("^p"      "ᵖ")
   ("^r"      "ʳ") ("^s"      "ˢ") ("^t"      "ᵗ") ("^u"      "ᵘ")
   ("^v"      "ᵛ") ("^w"      "ʷ") ("^x"      "ˣ") ("^y"      "ʸ")
   ("^z"      "ᶻ") ("_("      "₍") ("_)"      "₎") ("_+"      "₊")
   ("_-"      "₋") ("_0"      "₀") ("_1"      "₁") ("_2"      "₂")
   ("_3"      "₃") ("_4"      "₄") ("_5"      "₅") ("_6"      "₆")
   ("_7"      "₇") ("_8"      "₈") ("_9"      "₉") ("_="      "₌")
   ("_a"      "ₐ") ("_e"      "ₑ") ("_h"      "ₕ") ("_i"      "ᵢ")
   ("_j"      "ⱼ") ("_k"      "ₖ") ("_l"      "ₗ") ("_m"      "ₘ")
   ("_n"      "ₙ") ("_o"      "ₒ") ("_p"      "ₚ") ("_r"      "ᵣ")
   ("_s"      "ₛ") ("_t"      "ₜ") ("_u"      "ᵤ") ("_v"      "ᵥ")
   ("_x"      "ₓ"))
  (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
        (append math-symbol-list-basic math-symbol-list-extended)))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode))
(use-package markdown-mode+)
(use-package pandoc-mode :diminish)

(use-package smooth-scrolling :init (smooth-scrolling-mode))
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (expand-file-name ".emacs.keyfreq" user-emacs-directory)))

(use-package yasnippet
  :diminish yas-minor-mode)
(use-package yasnippet-snippets)
(use-package haskell-snippets)

(use-package ivy
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("C-r" . ivy-previous-line-or-history))
  :config
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-on-del-error-function 'nil
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist 'nil))

(use-package ivy-hydra)
(use-package ivy-yasnippet)
(use-package ivy-prescient
  :diminish
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))
(use-package ivy-dired-history)


(use-package counsel
  :diminish counsel-mode
  :bind
  (("M-x" . counsel-M-x)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable))
  :config
  (counsel-mode 1))

(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper) ;; questionable - should be temporary while I acclimate
   ))

(use-package counsel-projectile)

;; Python stuff
(use-package py-autopep8)
(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))


(use-package ripgrep
  :config
  (define-advice ripgrep-regexp 
      (:after (_ _ &optional _) switch-to-win)
    "Switch to the ripgrep result buffer after running the search
All arguments are ignored by this advice, but seem to be
necessary for the advice system"
    (let ((buf-name
           (compilation-buffer-name "ripgrep-search"
                                    'ripgrep-search-mode
                                    nil)))
      (pop-to-buffer buf-name))))


(use-package projectile
  :config
  (progn
    (projectile-mode)
    (setq projectile-completion-system 'ivy
          projectile-enable-caching t))) ;; Good even with alien
(use-package projectile-ripgrep)

(use-package docker)
(use-package dockerfile-mode)
(use-package docker-tramp)

(use-package elm-mode)
(use-package go-mode)
(use-package treemacs
  :config
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode)
  (setq treemacs-show-hidden-files nil))


(use-package system-packages
  :config
  (setq system-packages-package-manager 'apt
        system-packages-use-sudo t))

(use-package sicp)


(use-package zop-to-char
  :bind (("M-z" . zop-to-char)))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))


;; Nice set of non-offensive themes
(use-package doom-themes)


;; Local "packages"
(let ((theme-dir (expand-file-name "lisp/themes" "~/.emacs.d")))
  (add-to-list 'custom-theme-load-path theme-dir))


(use-package my-utils
  :demand
  :ensure f
  :load-path my/lisp-dir
  :bind ("M-Q" . my/unfill-paragraph)
  :config (progn 
            (require 'pandoc-mode)
            (push '("lines" . my/pandoc-include-lines) pandoc-directives)
            (push '("tag" . my/pandoc-include-tag) pandoc-directives)))


;; for Ackley's Living Computation course. Java-derived major-mode
(use-package ulam-mode
  :ensure f
  :demand
  :load-path my/lisp-dir
  :config
  (defun ulam/make () (interactive) (compile "make -k"))
  (defun ulam/run (&optional args)
    (interactive "smfzrun arguments:\n")
    (let ((cmd (format "make run ARGS=%s"
                       (shell-quote-argument args))))
      (compile cmd)))
  :bind (:map ulam-mode-map
              ("C-c C-k" . ulam/make)
              ("C-c C-r" . ulam/run)))



;; Use-package stuff ends here.  Below is more standard Elisp config


;; Always move to help window after opening (easier to close)
(setq help-window-select t)

;; Prefer horizontal (i.e. [L] | [R]) splits, without making stupidly narrow
;; windows
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; Don't prompt when reverting PDFs
(setq revert-without-query '(".*\\.pdf"))

;; Do not ping known domains when finding file at point
(setq ffap-machine-p-known "reject")

;; Don't ask about following links to source-controlled files -- just do it
(setq vc-follow-symlinks t)

;; Personal global keybindings
(mapcar
 (lambda (pr) (bind-key (car pr) (cdr pr)))
 '(
   ;; Simpler buffer and window nav
   ("C-<tab>"                 . other-window)
   ("<C-iso-lefttab>"         . (lambda () (interactive) (other-window -1)))
   ("M-["                     . previous-buffer)
   ("M-]"                     . next-buffer)

   ;; More convenient than M-DEL (DEL == backspace)
   ("M-D"                     . backward-kill-word)
   
   ;; Slightly quicker Kill this buffer
   ("C-x k"                   . kill-this-buffer)

   ;; I'm always aligning things
   ("C-M-;"                   . align-regexp)

   ;; Usually like fullscreen .  Don't know how to start an emacsclient-created
   ;; frame in fullscreen
   ("s-<return>"              . toggle-frame-fullscreen)

   ;; Get into eshell quicker
   ("C-S-t"                   . eshell)
   ;; Get into ansi-term quicker
   ("M-T"                     . my/ansi-term-zsh)

   ("C-M-}"                   . enlarge-window-horizontally)
   ("C-M-{"                   . shrink-window-horizontally)))

;; Interesting quirk of emacs - Ctrl+Shift vs Meta+Shift:
;; eval this:
;; (mapcar #'kbd '("C-T" "C-t" "C-S-t" "M-T" "M-t" "M-S-t"))
;; to get this:
;; ("" "" [33554452] [134217812] [134217844] [167772276])
;;
;; This makes it seem like {C,M}-t and {C,M}-T are identical as far as keyboard
;; input is concerned, so you'd think you'd want to use {C,M}-S-t to bind
;; {Ctrl,Meta}+Shift+t to something.  But checking those inputs with
;; `describe-key' reveals that, when I press Ctrl+Shift+t, Emacs interprets it
;; as C-S-t while Meta+Shift+t is interpreted as M-T, at least as far as keymaps
;; are concerned.  It's not really clear what's happening here, but it's the
;; reason for the inconsistent notation above.


;; Be Lazy, prefer Y or N to Yes or No
(fset 'yes-or-no-p 'y-or-n-p)

;; Bars suck
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Don't really like big fringes much either
(set-fringe-mode '(0 . 0))

;; disable bell and screen flashing
(defun my/do-nothing () nil)
(setq ring-bell-function 'my/do-nothing)

;; Make the fringes smaller
(setq default-left-fringe-width  5
      default-right-fringe-width 5)

;; prefer vertical splits
(setq-default split-height-threshold 80
              split-width-threshold 160)

;; Make paste over selection *replace* the selection
(delete-selection-mode)

;; Column numbers are good though
(column-number-mode)

;; Good to know where the cursor is
(global-hl-line-mode)

;; Basic indentation rules
(setq-default indent-tabs-mode nil
              tab-width 2
              indent-line-function 'insert-tab)

(setq-default fill-column 80)

;; Whitespace
(setq whitespace-style '(face tabs lines-tail empty trailing))


;; Monospaced font with great Unicode support for mathy symbols
;; http://www.evertype.com/emono/
(defconst font-everson-mono "Everson Mono-12")
;; Prettier font that scales down much better
(defconst font-office-code-pro "Office Code Pro-10")

(add-to-list 'default-frame-alist `(font . ,font-office-code-pro))
;; note for future me: backtick permits use of commas for evaluation inside a
;; quoted thing
;; Since I will probably forget this eventually:
;;  `M-x set-frame-font` is good (with completion) for switching between
;;  typefaces but not so much for choosing a different size unless you know the
;;  exact font spec string.
;;  (e.g. "-NATH-Office Code Pro-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
;;  The best way to change the full font is to use `set-face-attribute' as far as
;;  I can tell.  e.g. (set-face-attribute 'default nil :font "Office Code Pro-10"



;; make Proced auto-update
(setq proced-auto-update-flag t)

;; Don't like the startup screen
(setq inhibit-startup-screen t)

;; Don't warn about downcase regigion (C-x C-l)
(put 'downcase-region 'disabled nil)


;; Make C look the way I want it to
(setq c-default-style "linux"
      c-basic-offset 2)


;; default to modern fortran mode
(add-to-list 'auto-mode-alist '("\\.F\\'" . f90-mode))


;; Backup policy
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Help emacs print Unicode stuff?
(setq ps-multibyte-buffer :bdf-font-except-latin)
(setq bdf-directory-list "/usr/share/emacs/fonts/bdf")

;; Keep Custom elsewhere
(defconst my-custom-file (expand-file-name "lisp/my-custom.el" "~/.emacs.d"))
(setq custom-file my-custom-file)
(load custom-file)


;; let Custom declare this safe before loading it
(load-theme 'doom-spacegrey)

