;; -*- lexical-binding: t -*-


;; Make sure we've got straight
(load (locate-user-emacs-file "lisp/get-straight.el"))

;; This bit helps optimize startup time and is stolen+modified from
;; John Wiegley's config: github.com/jwiegley/dot-emacs
(defvar file-name-handler-alist-old file-name-handler-alist
  "Initial file-name-handler-alist at emacs startup")

(setq
 package-enable-at-startup  nil
 file-name-handler-alist    nil
 message-log-max            (* 16  1024)
 gc-cons-threshold          (* 256 1024 1024)
 gc-cons-percentage         0.6
 auto-window-vscroll        nil)

(add-hook
 'after-init-hook
 (defun my/restore-after-init()
   (interactive)
   (setq
    file-name-handler-alist  file-name-handler-alist-old
    gc-cons-threshold        (* 80 1024 1024)
    gc-cons-percentage       0.1)
   (garbage-collect))
 t)


;; My elisp is here
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Get the simple stuff right away
(require 'my-simple)

;; sets :straight t in all use-package decls
(setq straight-use-package-by-default t)


(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(straight-use-package 'diminish)


(setq
 ;; get more info from use-package - good for newbs like me
 use-package-verbose       t

 ;; Defer all pacakges by default. This means I have to be extra careful to
 ;; :demand those packages that I want loaded all the time.  This makes sense
 ;; for someone like me who tends to install too many packages for niche uses
 ;; (e.g. ssh-config-mode)
 use-package-always-defer  t)

;; suggested by jwiegley
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish (I do)
(require 'bind-key) ;; if you use any :bind variant (I do)

(use-package auto-compile
  ;; :straight t
  :config (auto-compile-on-load-mode))

;; Do this early, in case other packages want something in my $PATH at
;; load/install time
(use-package exec-path-from-shell
  :demand
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "WORKON_HOME")
        exec-path-from-shell-debug t)
  (exec-path-from-shell-initialize))


(use-package projectile
  :demand
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t ;; Good even with alien listing
        projectile-mode-line-prefix " ℙ"
        projectile-switch-project-action 'projectile-vc)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package rg
  :demand
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu)
  (setq
   ;; Show the full command in the *rg* buffer
   rg-hide-command nil
   ;; always search in compressed files files
   rg-command-line-flags (list "-z"))
  (defun my/rg-counsel-action (path)
    "Ripgrep in a directory/file, meant for counsel dispatch"
    (require 'f)
    (let ((dir (cond
                ((f-dir? path) path)
                ((f-file? path) (f-dirname path))
                t (error "%s isn't a file or directory :(" path)))
          (pat (rg-read-pattern nil))
          (files (rg-read-files)))
      (rg-run pat files dir)))
  (rg-define-search rg-)
  :bind (:map rg-mode-map
              ;; unbind nav conventions that I like
              ;; TODO: consider rebinding the history commands
              ("C-f") ("C-b") ("C-n") ("C-p")
              ;; moves cursor only
              ("n" . rg-next-file)
              ("p" . rg-prev-file)
              ;; moves cursor and navigates to point in relevant file
              ("M-n" . next-error-no-select)
              ("M-p" . previous-error-no-select)))

(use-package ivy
  :demand
  :diminish ivy-mode
  ;; :pin melpa
  :bind  (("C-c C-r" . ivy-resume))
  :config 
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-on-del-error-function 'nil
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist 'nil
        ivy-use-virtual-buffers t)
  (when (boundp 'set-message-function)
    ;; emacs@485b423e8f introduced some changes to how messages are set and
    ;; cleared in the minibuffer. By default, `set-message-function' is
    ;; `set-minibuffer-message', but this messes with how `ivy-dispatching-done'
    ;; displays the set of dispatch options.  Setting the variable to nil
    ;; restores the old behavior.
    (setq set-message-function nil)))

(use-package ivy-hydra)

(use-package swiper
  :demand)
(use-package counsel
  :demand
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
         ;; Note: this overrides `Info-goto-emacs-command-node'
         ("C-h F" . counsel-faces)
         ("C-s" . counsel-grep-or-swiper)
         ("C-c i" . counsel-imenu)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :config 
  ;; Suggested by Oleh
  (setq counsel-grep-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        ;; seems to be a fine limit in my use. The bigger I can get away with,
        ;; the better: line numbers are messed up when external grep is used
        counsel-grep-swiper-limit 2000000)
  (ivy-add-actions 'counsel-find-file
                   '(("v" projectile-vc "VC Status")
                     ("s" my/rg-counsel-action "ripgrep")))
  (counsel-mode 1))

(use-package counsel-projectile)

(use-package org
  :ensure org-plus-contrib
  :config
  (progn
    (setq ;; latexmk is a little more consistent than pdflatex
          org-latex-pdf-process (list "latexmk -f -pdf %f"))
    ;; make #+NAME easy-template
    (add-to-list 'org-structure-template-alist
                 '("n" . "name"))
    (add-to-list 'org-structure-template-alist
                 '("ct" . "clocktable"))))

(use-package ox-gfm)
(use-package ox-pandoc)


;; asynchronous execution of src blocks in org via babel
(use-package ob-async)

(defun my/org-babel-load-langs ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python     . t)
     (ditaa      . t)
     (emacs-lisp . t)
     (shell      . t))))

(add-hook 'after-init-hook 'my/org-babel-load-langs)

;; Undo-tree is great - enable it globally and remove it from the modeline
;; (since it should always be active)
(use-package undo-tree
  :demand
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package transient
  ;; keep transient in sync with magit on melpa
  :config (transient-bind-q-to-quit))

;; (use-package pinentry
;;   :init
;;   (setq epa-pinentry-mode 'loopback))

(use-package magit
  :demand
  :bind
  (("M-G"     . magit-status)
   ("C-x v B" . magit-blame-addition)
   :map magit-mode-map
   ("<tab>"   . magit-section-cycle))
  :config
  ;; (pinentry-start)
  ;; This prevents Magit from trying to intelligently restore a window
  ;; configuration - useful when you tend to do any amount of window changing
  ;; while magit buffers are visible
  (setq magit-bury-buffer-function 'magit-mode-quit-window
        magit-clone-default-directory "~/src/"))

;; link to Magit buffers in Org-mode
;; TODO: customize how export works for these links
;; (use-package orgit
;;   :after (magit org))

;; Comes with gitignore, gitconfig, gitattributes
(use-package git-modes)

(use-package clojure-mode)
(use-package cider)

;; (use-package haskell-mode)
;; (use-package intero
;;   :after haskell-mode
;;   :hook haskell-mode)

(use-package company
  :demand
  :hook (c++-mode . company-mode)
  :config
  (defun my/company-next () (interactive) (company-complete-common-or-cycle 1))
  (defun my/company-prev () (interactive) (company-complete-common-or-cycle -1))
  :bind (:map company-active-map
              ("M-C-i" . company-complete)
              ("C-c h" . company-quickhelp-manual-begin)
              ("C-n"   . my/company-next)
              ("C-p"   . my/company-prev)))

(use-package pass)
(use-package vterm)

(use-package smartparens
  :demand
  :init (setq sp-base-key-bindings 'paredit)
  :bind (:map smartparens-mode-map
              ("C-M-k" . sp-kill-sexp)
              ("C-M-<backspace>" . sp-backward-kill-sexp))
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode)
         (yaml-mode . smartparens-mode))
  :config
  ;; don't do anything with single-quote for elisp  
  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "'" nil :actions nil)
    ;; pair for identifier references in comments and docstrings
    (sp-local-pair "`" "'"
                   :when '(:add sp-in-comment-p sp-in-docstring-p)))
	(sp-with-modes 'markdown-mode
		(sp-local-pair "```" "```")
		(sp-local-pair "*" "*")
		(sp-local-pair "_" "_")
		(sp-local-pair "$" "$"))
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "#ifdef" "#endif")
    (sp-local-pair "#if" "#endif")
		(sp-local-pair "/*" "*/")))


;; Cleans up whitespace on save.
(use-package whitespace-cleanup-mode
  :hook prog-mode
  :diminish )

;; (use-package writeroom-mode)

(use-package pdf-tools
  :if (display-graphic-p)
  :demand
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config 
  (pdf-tools-install))

(use-package google-this
  :bind-keymap ("C-c g" . google-this-mode-submap))


(use-package lsp-mode
  :demand
  ;; :pin melpa
  :config
  (setq lsp-prefer-flymake nil
        lsp-keep-workspace-alive nil
        lsp-enable-snippet nil)
  :bind (:map lsp-mode-map ("C-c l r" . lsp-rename)))


(use-package company-lsp
  :after lsp-mode
  ;; :pin melpa
  ;; snippets don't seem to work too well.  At least, I can't figure out how to
  ;; expand them properly
  :config (setq company-lsp-enable-snippet t))

(use-package lsp-ui
  :after lsp-mode
  ;; :pin melpa
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil))

;; Dart + Flutter config
(use-package dart-mode)
(use-package lsp-dart 
  :hook (dart-mode . lsp))

(use-package rust-mode
  :hook (rust-mode . lsp))


;; C++ stuff
(use-package ccls
  :after lsp
  ;; Definitely need lsp for ccls to work
  :config
  (require 'json)
  (setq ccls-initialization-options
        (json-encode
         '((index . ((multiversion . 1))))))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (lambda () (cons ccls-executable ccls-args)))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil
    :remote? t)))

(defun my/maybe-enable-c++-lsp-server ()
  (interactive)
  (when (locate-dominating-file default-directory "compile_commands.json")
    (condition-case nil
        (progn
          (require 'ccls)
          (message "enabling c++ lsp server")
          (lsp))
      (user-error nil))))

(use-package clang-format
  :commands (clang-format clang-format-region)
  :init
  (with-eval-after-load 'cc-mode
    (fset 'c-indent-region 'clang-format-region)
    (fset 'c-indent-line-or-region 'clang-format)))

(add-hook 'c++-mode-hook #'my/maybe-enable-c++-lsp-server)
(add-hook 'c++-mode-hook #'hs-minor-mode)

(use-package cmake-mode)

(use-package js2-mode
  :config
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
  :diminish
  :init
  (add-to-list 'auto-mode-alist '("[Cc]omponents\\/.*\\.js\\'" . rjsx-mode)))

(use-package nim-mode)

(use-package json-mode :diminish)

(use-package markdown-mode
  :custom (markdown-asymmetric-header t "prefer just one set of #s")
  :hook ((markdown-mode . pandoc-mode)
         (markdown-mode . smartparens-mode))
  :config
  ;; 
  (set-face-attribute 'markdown-code-face nil
                      :background 'unspecified
                      :inherit 'font-lock-constant-face))

(use-package edit-indirect)

(use-package pandoc-mode
  :diminish)

(use-package tablist
  :config
  (add-hook 'tabulated-list-mode-hook 'tablist-minor-mode)
  :bind (:map tablist-minor-mode-map
              ("U" . nil)))

(use-package helpful
  :demand
  :bind
  (:map help-map
        ("f"       . helpful-callable)
        ("v"       . helpful-variable)
        ("k"       . helpful-key)
        ("C-<SPC>" . helpful-at-point)))

(use-package prescient
  :demand
  :config
  (setq prescient-save-file
        (expand-file-name ".cache/prescient-save.el" user-emacs-directory))
  (prescient-persist-mode 1))
(use-package ivy-prescient
  :diminish
  :demand
  :config
  (ivy-prescient-mode 1))

;; 
(use-package direnv)

;; Python stuff
(use-package py-autopep8)

;; Support for python virtual environments.  This requires the environment
;; variable WORKON_HOME to be set as the location of python environments.
;; Ideally, this should be set in the environment emacs is invoked in, or picked
;; up by exec-path-from-shell
(use-package pyvenv)

;; Protocol buffer support
(use-package protobuf-mode)

(use-package editorconfig
  :diminish
  :config (editorconfig-mode 1))

(use-package docker
  :bind (("C-c d" . docker)))
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp
  ;; Using names (rather than IDs) is a good idea.  It makes it easier to
  ;; identify containers when autocompleting for the docker method, and tramp
  ;; persistency information (in `tramp-persistency-file-name') is no longer
  ;; tied to an ephemeral ID that is invalided when containers are removed.
  ;; This could cause issues if a name is reused for wildly different
  ;; containers, but that's not generally an issue in my use case.
  :config (setq docker-tramp-use-names t))

(use-package ace-window
  ;; :pin melpa
  :init
  (setq
   aw-keys '(?a ?s ?d ?f ?j ?k ?l)

   ;; Necessary rebind since I use the default (z) in the dispatch alist
   ;; aw-make-frame-char ?W

   ;; This alist is close to the default - just a few tweaks to avoid collision
   ;; with `aw-keys' and for personal preferences.  I could `setf' with
   ;; `alist-get' here, but that makes it less clear what's bound, and might
   ;; break if the defaults change.
   ;; aw-dispatch-alist
   ;; '((?x aw-delete-window                "Delete Window")
   ;;   (?m aw-swap-window                  "Swap Windows")
   ;;   (?M aw-move-window                  "Move Window")
   ;;   (?c aw-copy-window                  "Copy Window")
   ;;   (?b aw-switch-buffer-in-window      "Select Buffer") ;; me
   ;;   (?n aw-flip-window                  "Select Previous Window")
   ;;   (?u aw-switch-buffer-other-window   "Switch Buffer Other Window")
   ;;   (?e aw-execute-command-other-window "Execute Command Other Window")
   ;;   (?S aw-split-window-fair            "Split Window DWIM") ;; me
   ;;   (?v aw-split-window-vert            "Split Window Vertically")
   ;;   (?z aw-split-window-horz            "Split Window Horizontally") ;; me
   ;;   (?o delete-other-windows            "Delete Other Windows")
   ;;   (?? aw-show-dispatch-help           "Show this help menu"))
   )
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :height 5.0
                      :foreground 'unspecified
                      :inherit 'error)
  :bind (("C-x o" . 'ace-window)))

(use-package expand-region
  :demand
  :bind (("M-@" . er/expand-region)))

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode))
  :config
  (add-to-list 'hl-todo-keyword-faces
               '("WIP"  . "#d0bf8f")))

(use-package ssh-config-mode)
(use-package crontab-mode)
(use-package nov
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-variable-pitch nil)
  (setq nov-text-width 72))

;; SICP as a texinfo document
(use-package sicp)

(use-package which-key
  :demand
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))


;; Stolen from u/pkkm: https://www.reddit.com/r/emacs/comments/4v7tcj/-/d5wyu1r/
;; This is useful when e.g. setting color variables based on the current loaded
;; theme.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(advice-add
 'load-theme :after
 (defun run-load-theme-hooks (theme &rest ignored)
   "Run hooks in `after-load-theme-hook'"
   (run-hook-with-args 'after-load-theme-hook theme)))


;; Sources for themes I like
(use-package base16-theme
  :init (setq base16-theme-256-color-source 'colors)
  :demand
  :config
  (setq my/base16-last-theme-idx -1)
  (setq my/base16-all-themes
        (progn
          (require 'dash)
          (--filter (string-match-p "^base16" (symbol-name it))
                    (custom-available-themes))))
  (defun my/base16-next-theme ()
    "Cycle through base16 themes"
    (interactive)
    (setq my/base16-last-theme-idx
          (if (or (< my/base16-last-theme-idx 0)
                  (>= my/base16-last-theme-idx (1- (length my/base16-all-themes))))
              0
            (1+ my/base16-last-theme-idx)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (nth my/base16-last-theme-idx my/base16-all-themes) t))
  
  (defcustom my/base16-inhibit-patch nil
    "If nil, base16 themes will be patched on switch.
Patches are made in the `after-load-theme-hook', and are
determined by the variable `my/base16-patchset'")

  (defcustom my/base16-patchset
    '((org-todo :background base01)
      (org-done :background base01)
      ;; Make mode line more prominent to highlight active window
      (mode-line :background base05 :foreground base03)
      (mode-line-buffer-id :foreground base01 :weight bold)
      ;; Make header line distinguishable from modeline
      (header-line :background base03)
      (undo-tree-visualizer-current-face :foreground base00
                                         :background base0B)
      ;; Prefer something slightly more prominent for window borders
      ;; NOTE: does not effect unused `window-divider-mode'
      (vertical-border :foreground base0A))
    "List of face specs to be passed to `base16-patch-theme'
    whenever a base16 theme is loaded")

  (defun my/get-base16-color-plist (theme)
    (let ((color-var (intern (concat (symbol-name theme) "-colors"))))
      (if (boundp color-var)
          (symbol-value color-var)
        (error "%s is probably not a base16 theme or is not loaded" theme))))

  (defun my/get-base16-enabled-theme ()
    "Return the first enabled base16 theme or nil"
    (interactive)
    (-find (lambda (theme)
             (save-match-data
               (string-match "base16-" (symbol-name theme))))
           custom-enabled-themes))

  (defun base16-patch-theme (theme faces)
    "Apply changes in FACES to THEME, a `base16-theme'.
FACES should take same form as in `base16-theme-define'. If THEME
is nil, the first loaded base16 theme is used (see
`my/get-base16-enabled-theme'). If no such theme is loaded,
calling this function is a NOP"
    (when
        ;; allow nil arg => get current base16 theme
        (setq theme (or theme (my/get-base16-enabled-theme)))
      (let ((colors (my/get-base16-color-plist theme)))
        (dolist (spec faces)
          ;; prefer set-face-attribute over base16-set-faces because it preserves
          ;; any existing face attributes
          (apply
           'set-face-attribute
           `(,(car spec) nil ,@(base16-transform-spec (cdr spec) colors)))))))

  ;; add the hook to patch base16 themes on load
  (add-hook
   'after-load-theme-hook
   (defun patch-loaded-base16-theme (theme)
     (message "loaded %S" theme)
     (when  (and (string-match "^base16-" (symbol-name theme))
                 (not my/base16-inhibit-patch))
       (message "patched %S" theme)
       (base16-patch-theme theme my/base16-patchset))))

  ;; Actually load a theme
  (let ((theme 'base16-default-dark))
    (require 'org-faces)
    (load-theme theme t)))

(use-package rainbow-mode
  :init
  (add-hook
   ;; If we're visiting an elisp theme file (usually globbed as *-theme.el) turn
   ;; on rainbow mode
   'find-file-hook
   (defun enable-rainbows-for-theme ()
     "Enables `rainbow-mode' if the current file is a theme file.
A file is considered a theme file if it matches the regex
\"-theme.el\".  Meant to be used as a `find-file-hook'"
     (when (string-match-p "-theme.el" (or buffer-file-name ""))
       (rainbow-mode 1))))
  :config  
  (add-hook
   'rainbow-mode-hook
   (defun my/add-base16-rainbow-highlights ()
     (font-lock-add-keywords
      nil
      '(("\\<:?\\(base0[0-9A-F]\\)\\>"
         (0  ;; <-- defining face for group 0, i.e. the whole regex
          (when-let*
              (;; get a symbol we can use with the plist (e.g. :base04)
               (match-sym (intern (concat ":" (match-string 1))))
               (theme (my/get-base16-enabled-theme))
               (colors (my/get-base16-color-plist theme))
               (color (plist-get colors match-sym)))
            `((:foreground
               ;; similar to logic in rainbow-mode for lightness
               ,(if (> 0.5 (caddr (apply #'color-rgb-to-hsl
                                         (color-name-to-rgb color))))
                    "white" "black"))
              (:background ,color)))
              ;; (rainbow-colorize-by-assoc str-alist)
              )))
      t))))

;; When in a terminal, prefer the simple builtin tsdh-dark theme over base16 (at
;; least until I figure out how to make base16 themes look decent in the
;; terminal)
(when (not (display-graphic-p))
  (load-theme 'tsdh-dark t)
  (set-face-attribute 'mode-line nil :family '(:inherit default)))

;; Info-mode
(use-package info
  :init
  (defvar my/info-scroll-margin 25
    "Least number of lines must be visible before a scroll causes
    navigation to the next Info node")
  (defun my/info-next ()
    "Behave as `Info-next' when viewing single node in a manual
or as `forward-page' when viewing the whole manual (as after
invoking \"g *\")"
    (interactive)
    ;; checking if buffer is narrowed is the most reliable way of detecting how
    ;; the info buffer is viewing its manual
    (if (buffer-narrowed-p)
        (Info-next)
      (forward-page)))
  (defun my/info-prev ()
    "Behave as `Info-prev' when viewing single node in a manual
or as `backward-page' when viewing the whole manual (as after
invoking \"g *\")"
    (interactive)
    ;; checking if buffer is narrowed is the most reliable way of detecting how
    ;; the info buffer is viewing its manual
    (if (buffer-narrowed-p)
        (Info-prev)
      (backward-page)))
  ;; functions like scroll-{up,down}-line but will move to next/prev info page
  ;; if the last line of the buffer is within `my/info-scroll-margin' of the
  ;; window start or the window start line is the same as the buffer start line
  ;; respectively
  (defun my/info-scroll-down-line ()
    (interactive)
    (if (= 1 (line-number-at-pos (window-start)))
        (Info-last-preorder)
      (scroll-down-line)))
  (defun my/info-scroll-up-line ()
    (interactive)
    (let* (;; The *real* last line appears to be off by two, due to how a line
           ;; with character 0x1F (INFO SEP 1) is treated for line numbering
           ;; purposes
           (last-line (- (line-number-at-pos (window-end)) 2))
           (lim (+ (line-number-at-pos (window-start)) my/info-scroll-margin)))
      (if (<= last-line lim)
          (or (Info-no-error (Info-goto-node (Info-extract-menu-counting 1)))
              (Info-next-preorder))
        (scroll-up-line))))
  :bind (:map Info-mode-map
              ("j" . my/info-scroll-up-line)
              ("k" . my/info-scroll-down-line)
              ("n" . my/info-next)
              ("p" . my/info-prev))
  :config
  ;; the default is `fixed-pitch-serif', which is bad by default for me in most
  ;; cases.  Probably could be fixed with fc-*, but this is simpler
  (set-face-attribute
   'Info-quoted nil :inherit 'font-lock-constant-face)
  (add-hook
   'Info-mode-hook
   (defun my/set-info-page-delim ()
     "Set `page-delimiter' locally for `Info-mode' to ASCII FS"
     ;; Info uses ^_ (File separator / INFORMATION SEPARATOR ONE) to delimit
     ;; nodes. Since `forward-page' and `backward-page' don't really have much
     ;; use in the normal single-node view, there's no need to restore the page
     ;; delimiter to its normal FORM FEED regex locally.
     (setq-local page-delimiter "^\^_"))))

;; Remember files with recentf
(use-package recentf
  :demand
  :config
  (setq recentf-max-menu-items 200)
  (recentf-mode 1))



;; Local "packages"
;; (use-package my-utils
;;   :demand
;;   :load-path my/lisp-dir
;;   :bind (("M-Q" . my/unfill-paragraph)
;;          ("C-x C-o" . my/other-win))
;;   :config (progn 
;;             (require 'pandoc-mode)
;;             (push '("lines" . my/pandoc-include-lines) pandoc-directives)
;;             (push '("tag" . my/pandoc-include-tag) pandoc-directives)))

;; Uncomment when ready to use mu4e
;; (use-package my-email
;;   :demand
;;   :load-path my/lisp-dir)



;; Use-package stuff ends here.  Below is more standard Elisp config


;; If there's an llvm-ish folder in the load path
(require 'cl-lib)
(when (cl-member-if (lambda (path)
                   (and (file-directory-p path)
                        (string-match "llvm" path)))
                 load-path)
  (require 'llvm-mode)
  (require 'tablegen-mode))

;; TODO: Try to make this work.
;;
;; Goal is to make it so that lines with TODO: or NOTE: are considered their own
;; paragraph for the purpose of fill-paragraph.
(add-hook 'prog-mode-hook
          (defun my/set-paragraph-sep-for-todos ()
            "Set `paragraph-separate'"
            (unless (boundp 'my/-set-para)
              (setq-local paragraph-separate
                          (concat "[[:space:]]*\\(NOTE\\|TODO\\):.*$" "\\|"
                                  paragraph-separate))
              (setq-local paragraph-start
                          (concat "[[:space:]]*\\(NOTE\\|TODO\\):.*$" "\\|"
                              paragraph-start))
              (setq-local my/-set-para t))))


;; Better alignment when using tabby modes
(advice-add
 'align-regexp
 :around
 (defun fix-indent-for-align-rx (origfn &rest args)
   "Turn off indent-tabs-mode when aligning.
Poor man's smart-tabs, but maybe more reliable?"
   (let ((indent-tabs-mode nil))
     (apply origfn args))))

;; Better buffer handing for {async-,}shell-command
(advice-add
 'shell-command
 :after
 (defun shell-rename-buffer (cmd &optional o-buf e-buf)
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
               
(advice-add
 'man
 :around
 (defun make-man-pushy (origfn &rest args)
   "Advice to make `man' reuse the window when called from a
`Man-mode' buffer"
   (let ((Man-notify-method
          (if (equal major-mode 'Man-mode)
              'pushy
            Man-notify-method)))
     (apply origfn args))))


;; Tramp should use SSH by default (it's usually faster than SCP).
(setq tramp-default-method "ssh")
;; Let tramp use the PATH set on the remote (NOTE: tramp checks the path in a
;; shell invoked as "sh -l")
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Don't use a new frame for ediffing, split horizontally by default
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Save/restore windows around ediff sessions
;; stolen from https://emacs.stackexchange.com/a/17089/19865
(defvar my/ediff-last-windows nil)
(defun my/store-pre-ediff-winconfig ()
  (setq my/ediff-last-windows (current-window-configuration)))
(defun my/restore-pre-ediff-winconfig ()
  (set-window-configuration my/ediff-last-windows))
(add-hook 'ediff-before-setup-hook #'my/store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my/restore-pre-ediff-winconfig)



;; Personal global keybindings
(bind-keys
 ;; :map global-map
 ;; TODO: remap these or only use them when (display-graphic-p)
 ;; They do not play well with terminals.
 ;; ("M-["                     . previous-buffer)
 ;; ("M-]"                     . next-buffer)
 
 ;; More convenient than M-DEL (DEL == backspace)
 ("M-D"                     . backward-kill-word)
 ("C-x C-b"                 . ibuffer)

 ;; I'm always aligning things
 ("C-M-;"                   . align-regexp)

 ;; Usually like fullscreen .  Don't know how to start an emacsclient-created
 ;; frame in fullscreen
 ("s-<return>"              . toggle-frame-fullscreen)

 ;; Get into eshell quicker
 ("C-S-t"                   . eshell)

 
 ([remap eval-expression]   . pp-eval-expression)
 ([remap eval-last-sexp]    . pp-eval-last-sexp)

 :map kmacro-keymap ;; default bound to C-x C-k prefix
 
 ;; From function docs: "You can call the macro again by repeating just the last
 ;; key in the key sequence used to call this command".
 ("c" . kmacro-call-macro))
;; Interesting quirk of emacs - Ctrl+Shift vs Meta+Shift:
;; eval this:
;; (mapcar #'kbd '("C-T" "C-t" "C-S-t" "M-T" "M-t" "M-S-t"))
;; to get this:
;; ("" "" [33554452] [134217812] [134217844] [167772276])
;;
;; It's strange that C-{T,t} seem to be recognized the same by `kbd', but
;; M-{T,t} do not. Checking those inputs with `describe-key' reveals that, when
;; I press Ctrl+Shift+t, Emacs interprets it as C-S-t while Meta+Shift+t is
;; interpreted as M-T, at least as far as keymaps are concerned.  It's not
;; really clear what's happening here, but it's the reason for the inconsistent
;; notation above.

;; add some some bindings for compilation mode
(defun my/compilation-goto-error-no-select ()
  "Goto error but don't leave the compilation buffer"
  (interactive)
  (compile-goto-error)
  (pop-to-buffer next-error-last-buffer))

(with-eval-after-load 'compile
  (bind-keys :map compilation-button-map
             ("Q" . kill-this-buffer)
             ("<S-return>" . my/compilation-goto-error-no-select)))


(defun my/make-file-mode-prop-line (&optional select)
  (interactive "p")
  (let ((the_mode
         (if select
             (let ((modes))
               (mapatoms
                (lambda (sym)
                  (when (string-match "-mode$" (symbol-name sym))
                    (add-to-list 'modes sym))))
               (completing-read
                "Mode (should be a *major* mode): " modes))
           major-mode)))
    (add-file-local-variable-prop-line
     'mode the_mode)))



;; Set scratch buffer custom message and make it an elisp buffer
(setq initial-scratch-message
      ";; -*- lexical-binding: t -*-
;; --------------- Emacs Lisp Scratch buffer --------------- ;;"
      initial-major-mode 'emacs-lisp-mode)


;; Whitespace
(setq whitespace-style '(face tabs lines-tail empty trailing))

;; Make `man' open pages in the other window and switch to that buffer
(setq Man-notify-method 'aggressive)


;; Font selection
(require 'dash)
(funcall
 (defun my/set-default-fonts ()
   "Set the default font to one of a few I like"
   (-when-let*
       ((_ (display-graphic-p))
        (fontlist
         '(("Hack"             :size 14 :weight normal)
           ("DejaVu Sans Mono" :size 14 :weight normal)
           ("Inconsolata"      :size 15 :weight normal)
           ("Office Code Pro"  :size 14 :weight normal)
           ("Everson Mono"     :size 14 :weight bold)))
        (font-entity (--reduce-from ;; \acc it ->
                      (or acc
                          (let ((spec (apply #'font-spec :name it)))
                            (when (find-font spec) spec)))
                      nil fontlist)))
     (set-face-attribute 'default nil :font font-entity))))


;; don't want plain-text auth storage, so just use gpg-encrypted source
(setq auth-sources '("~/.authinfo.gpg"))
(require 'auth-source-pass)
;; also enable password-store auth source
(auth-source-pass-enable)


;; from https://emacs.stackexchange.com/a/3157
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(bind-key "M-<tab>" 'hs-toggle-hiding hs-minor-mode-map)
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

;; make Proced auto-update
(setq proced-auto-update-flag t)


;; Make C look the way I want it to
(setq c-default-style "linux"
      c-basic-offset 2
      sh-basic-offset 2)


;; default to modern fortran mode
(add-to-list 'auto-mode-alist '("\\.F\\'" . f90-mode))

