;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq gc-cons-threshold (* 100 1024 1024))
(package-initialize)

;; Good to have some secrets
(let ((s-file (expand-file-name "secrets.el" "~/.emacs.d")))
  (when (file-exists-p s-file)
    (load s-file)))

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ;; ("marmalade"    . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")))

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
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; Undo-tree is great - enable it globally and remove it from the modeline
;; (since it should always be active)
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package magit
  :bind (("C-M-g" . magit-status)))
(use-package gitignore-mode)
(use-package intero)
(use-package haskell-mode
  :config (add-hook 'haskell-mode-hook 'intero-mode))
(use-package company-quickhelp)
(use-package company 
  :config
  (defun my/company-next () (interactive) (company-complete-common-or-cycle 1))
  (defun my/company-prev () (interactive) (company-complete-common-or-cycle -1))
  :bind (("M-<return>" . company-complete)
         :map company-active-map 
              ("C-c h" . company-quickhelp-manual-begin)
              ("C-n" . my/company-next)
              ("C-p" . my/company-prev)))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))
;; Note: Company-ghc seems to cause emacs to hang when writing in comments.
;; Not sure why though...
;; (use-package company-ghc
;;   :config
;;   (add-to-list 'company-backends 'company-ghc))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))



(use-package smart-mode-line)
(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (sp-with-modes 'markdown-mode
    (sp-local-pair "```" "```")
    (sp-local-pair "*" "*")
    (sp-local-pair "_" "_")
    (sp-local-pair "$" "$")  ; for pandoc LaTeX math extension
    (sp-local-pair "⟦" "⟧")) ; fancy brackets for semantic functions
  (sp-with-modes 'c++-mode
    (sp-local-pair "/*" "*/"))
  :bind (:map smartparens-mode-map
              ("C-c u w" . sp-unwrap-sexp)
              ("C-M-f"   . sp-forward-sexp)
              ("C-M-b"   . sp-backward-sexp)
              ("C-M-k"   . sp-kill-sexp)))

;; Whitespace-related
(use-package whitespace-cleanup-mode
  :config (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Show me where the cursor is, when it changes
(use-package beacon
  :init
  (beacon-mode 1)
  (setq beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-point-moves-vertically nil
        beacon-blink-when-window-scrolls 0
        beacon-color "navajo white"))

(use-package pdf-tools
  :config
  (pdf-tools-install))


;; C++ stuff


(use-package cmake-mode)
(use-package rtags)
(use-package flycheck-rtags
  :config
  (defun my/flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil)))

(use-package irony-eldoc) ; https://github.com/Sarcasm/irony-eldoc
(use-package flycheck-irony ; https://github.com/Sarcasm/flycheck-irony
  :init (eval-after-load 'flycheck
          '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
(use-package company-irony
  :init (eval-after-load 'company
          '(add-to-list 'company-backends 'company-irony)))
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook #'my/flycheck-rtags-setup)
(add-hook 'c++-mode-hook 'company-mode)
  
(use-package cmake-ide
  :config
  (require 'rtags)
  (cmake-ide-setup))

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

(use-package dokuwiki-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.dokuwiki\\'" . dokuwiki-mode)))

(use-package writeroom-mode
  :config
  (setq writeroom-width 100))
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
  (setq keyfreq-file (expand-file-name ".emacs.keyfreq" "~/.emacs.d/")))

(use-package yasnippet)
(use-package haskell-snippets)

(use-package helm
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h"     . helm-mini)
         ("C-h a"     . helm-apropos)
         ("C-x b"     . helm-buffers-list)
         ("C-x C-f"   . helm-find-files)
         ("M-y"       . helm-show-kill-ring)
         ("M-x"       . helm-M-x)
         :map helm-map
         ("<tab>"     . helm-execute-persistent-action)
         ("C-i"       . helm-execute-persistent-action)
         ("C-z"       . helm-select-action)))

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(use-package helm-ag)

(use-package helm-tramp)
(use-package helm-ghc)
(use-package helm-unicode)
(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))



(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;; Local "packages"
(let ((theme-dir (expand-file-name "lisp/themes" "~/.emacs.d")))
  (add-to-list 'custom-theme-load-path theme-dir))

(use-package my-utils
  :demand
  :ensure f
  :load-path "lisp/"
  :bind ("M-Q" . my/unfill-paragraph)
  :config
  (require 'pandoc-mode)
  (push '("lines" . my/pandoc-include-lines) pandoc-directives)
  (push '("tag" . my/pandoc-include-tag) pandoc-directives))

;; (use-package my-c-setup
;;   :ensure f
;;   :load-path "lisp/")

;; for Ackley's Living Computation course. Java-derived major-mode
(use-package ulam-mode
  :ensure f
  :demand
  :load-path "lisp/"
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

;; The normal exec-path fix doesn't work so well when emacs is started as a
;; server.  This might fix that? Note: doesn't seem to ...
(defun client-fix-path (_)
  (require 'exec-path-from-shell)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(add-hook 'after-make-frame-functions #'client-fix-path)

;; Always move to help window after opening (easier to close)
(setq help-window-select t)

;; Prefer horizontal (i.e. [L] | [R]) splits, without making stupidly narrow
;; windows
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; Don't prompt when reverting PDFs
(setq revert-without-query '(".*\\.pdf"))

;; Personal global keybindings
(mapcar
 (lambda (pr) (global-set-key (kbd (car pr)) (cdr pr)))        
 '(
   ;; Simpler buffer and window nav
   ("C-<tab>"         . other-window)
   ("<C-iso-lefttab>" . (lambda () (interactive) (other-window -1)))
   ("M-["             . previous-buffer)
   ("M-]"             . next-buffer)
   ;; Slightly quicker Kill this buffer
   ("C-x C-k"         . kill-this-buffer)
   ;; I'm always aligning things
   ("C-M-;"           . align-regexp)
   ;; Usually like fullscreen.  Don't know how to start an emacsclient-created
   ;; frame in fullscreen
   ("s-<return>"      . toggle-frame-fullscreen)
   ;; Get into eshell quicker
   ("C-S-t"           . eshell)
   ))

;; Be Lazy, prefer Y or N to Yes or No
(fset 'yes-or-no-p 'y-or-n-p)

;; Bars suck
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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

(setq whitespace-style '(face tabs lines-tail empty trailing))

;; Monospaced font with great Unicode support for mathy symbols
;; http://www.evertype.com/emono/
(defconst everson-mono "Everson Mono-11:bold")
(add-to-list 'default-frame-alist `(font . ,everson-mono))
;; note for future me: backtick permits use of commas for evaluation inside a
;; quoted thing

;; make Proced auto-update
(setq proced-auto-update-flag t)

;; Don't like the startup screen
(setq inhibit-startup-screen t)

;; Make C look the way I want it to
(setq c-default-style "linux"
      c-basic-offset 2)

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
(load-theme 'greymatters-dark)
