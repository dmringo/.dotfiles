;; -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 100 1024 1024))
(setq my/hostname (shell-command-to-string "printf %s $(hostname -s)"))

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
  (progn
    ;; ditaa path as installed by apt
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
          ;; latexmk is a little more consistent than pdflatex
          org-latex-pdf-process (list "latexmk -f -pdf %f"))
    ;; make #+NAME easy-template
    (add-to-list 'org-structure-template-alist '("n" "#+NAME: ?"))
    (add-to-list 'org-structure-template-alist
                 '("ct" "#+BEGIN: clocktable ?\n#+END:"))))

(use-package ox-twbs)
(use-package ox-gfm)
(use-package ox-pandoc)
(use-package ob-async)
(use-package htmlize)


(defun my/org-babel-load-langs ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ditaa . t)
     (emacs-lisp  . t)
     (shell . t))))

(add-hook 'after-init-hook 'my/org-babel-load-langs)

;; Undo-tree is great - enable it globally and remove it from the modeline
;; (since it should always be active)
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package magit
  :bind (("M-G" . magit-status)
         ("C-x v B" . magit-blame-addition)
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
  :init (setq sp-base-key-bindings 'sp)
  :config (progn
	    (add-hook 'prog-mode-hook 'smartparens-mode)
	    (sp-with-modes 'markdown-mode
			   (sp-local-pair "```" "```")
			   (sp-local-pair "*" "*")
			   (sp-local-pair "_" "_")
			   (sp-local-pair "$" "$"))
	    (sp-with-modes 'c++-mode
			  (sp-local-pair "/*" "*/"))))

;; Whitespace-related
(use-package whitespace-cleanup-mode
  :diminish
  :config (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

;; Good for Makefiles where actual tabs are important but alignment really ought
;; to be accomplished with spaces.
(use-package smart-tabs-mode)

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

(use-package ccls
  :commands lsp-ccls-enable)

(defun my/maybe-enable-c++-lsp-server ()
  (interactive)
  (when (locate-dominating-file default-directory "compile_commands.json")
    (condition-case nil
        (progn
          (message "enabling c++ lsp server")
          (lsp-ccls-enable))
      (user-error nil))))

(use-package clang-format
  :bind (:map c-mode-base-map
              ("C-M-\\" . clang-format-region)
              ("C-i" . clang-format))
  :config (fset 'c-indent-region 'clang-format-region))

(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook #'my/maybe-enable-c++-lsp-server)

(use-package google-c-style
  :config (c-add-style "Google" google-c-style))

(use-package cmake-mode)

(use-package comment-dwim-2
  :config (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(use-package prettier-js)
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
   ("_x"      "ₓ") ("\\lz"    "◊"))
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
  :bind  (("C-c C-r" . ivy-resume)
          :map ivy-minibuffer-map
          ("C-r" . ivy-previous-line-or-history))
  :config (progn
            (ivy-mode 1)
            (setq ivy-height 20
                  ivy-on-del-error-function 'nil
                  ivy-use-selectable-prompt t
                  ivy-initial-inputs-alist 'nil
                  ivy-use-virtual-buffers t)))

(use-package ivy-hydra)
(use-package swiper)
(use-package counsel
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-s" . counsel-grep-or-swiper))
  :config (progn
            ;; Suggested by Oleh
            (setq counsel-grep-command
                  "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
            (counsel-mode 1)))

(use-package helpful)
(use-package ivy-yasnippet)
(use-package ivy-prescient
  :diminish
  :config
  (progn
    (ivy-prescient-mode 1)
    (prescient-persist-mode 1)))
(use-package ivy-dired-history)

(use-package direnv)

;; Racket
(use-package racket-mode)

;; Python stuff
(use-package py-autopep8)
(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))


(use-package ripgrep
  :config
  (define-advice ripgrep-regexp 
      (:around (rg-orig regx dir &optional args) my/advice)
    "Advice for `ripgrep-regexp'.
This does two things: 

 - Switches to the ripgrep result buffer after running the search

 - Adds the ability to call `ripgrep-regexp' with arbitrary extra
   arguments (prompted in the minibuffer) when called with a
   prefix argument.
"
    (let ((buf-name (compilation-buffer-name "ripgrep-search"
                                             'ripgrep-search-mode
                                             nil))
          (smart-arg (if (consp current-prefix-arg)
                         (read-from-minibuffer "Extra rg args: ")
                       "")))
      (funcall rg-orig regx dir (nconc args (list smart-arg "-M 120")))
      (pop-to-buffer buf-name)))
  :bind ("C-c s r" . ripgrep-regexp))


(use-package projectile
  :config
  (progn
    (projectile-mode)
    (setq projectile-completion-system 'ivy
          projectile-enable-caching t)) ;; Good even with alien
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))


(use-package counsel-projectile)


(use-package projectile-ripgrep)

(use-package editorconfig
  :diminish
  :config (editorconfig-mode 1))

(use-package docker)
(use-package dockerfile-mode)
(use-package docker-tramp)

(use-package elm-mode)

(use-package go-mode)
(use-package go-eldoc)
(use-package company-go)
(use-package lsp-go
  :init
  (add-hook 'go-mode-hook #'lsp-go-enable))

(use-package treemacs
  :config
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode)
  (setq treemacs-show-hidden-files nil))


(use-package system-packages
  :config
  (setq system-packages-package-manager 'apt
        system-packages-use-sudo t))

(use-package ssh-config-mode)

;; SICP as a texinfo document
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
  :bind (("M-Q" . my/unfill-paragraph)
         ("C-c s p" . #'my/relativize-path-at-point))
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

;; Tramp should use SSH by default (it's usually faster than SCP)
(setq tramp-default-method "ssh")

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
   ("C-M-{"                   . shrink-window-horizontally)
   ("C-h M"                   . man)
   ([remap eval-expression]   . pp-eval-expression)
   ([remap eval-last-sexp]    . pp-eval-last-sexp)
   ))

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
              tab-width 2)

(setq-default fill-column 80)

;; Whitespace
(setq whitespace-style '(face tabs lines-tail empty trailing))

;; Make `man' open pages in the other window and switch to that buffer
(setq Man-notify-method 'aggressive)


;; Monospaced font with great Unicode support for mathy symbols
;; http://www.evertype.com/emono/
(defconst font-everson-mono "Everson Mono-12")
;; Prettier font that scales down much better
(defconst font-office-code-pro (cond
                                ;; TODO: pick font size based on DPI or
                                ;; something?  This is also a function of how
                                ;; far from my monitor I am... Can emacs measure
                                ;; real-world distance?
                                ((equal my/hostname "zarniwoop") "Office Code Pro-16")
                                (t "Office Code Pro-10")))


;; from https://emacs.stackexchange.com/a/3157
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)



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
      c-basic-offset 2
      sh-basic-offset 2)


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

