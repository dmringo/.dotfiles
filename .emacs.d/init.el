;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; Good to have some secrets
(let ((s-file (expand-file-name "secrets.el" "~/.emacs.d")))
  (when (file-exists-p s-file)
    (load s-file)))


(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("marmalade"    . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose       t
      use-package-always-ensure t)

;; suggested by jwiegly
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; Always move to help window after opening (easier to close)
(setq help-window-select t)


;; Personal global keybindings
(mapcar
 (lambda (pr) (global-set-key (kbd (car pr)) (cdr pr)))        
 '(
   ;; Simpler buffer and window nav
   ("C-<tab>" . other-window)
   ("<C-iso-lefttab>" . (lambda () (interactive) (other-window -1)))
   ("M-["     . previous-buffer)
   ("M-]"     . next-buffer)
   ;; Slightly quicker Kill this buffer
   ("C-x C-k" . kill-this-buffer)
   ;; I'm always aligning things
   ("C-M-;"   . align-regexp)
   )
 )

;; Be Lazy, prefer Y or N to Yes or No
(fset 'yes-or-no-p 'y-or-n-p)

;; Bars suck
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Make the fringes smaller
(setq default-left-fringe-width 5
      default-right-fringe-width 5)

;; prefer vertical splits
(setq-default split-height-threshold 80
              split-width-threshold 160)

;; Full-screen 4 lyfe
(toggle-frame-fullscreen)

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

;; Monospaced font with great Unicode support for mathy symbols
;; http://www.evertype.com/emono/
(defconst everson-mono "Everson Mono-13:bold")
(add-to-list 'default-frame-alist `(font . ,everson-mono))
;; note for future me: backtick permits use of commas for evaluation inside a
;; quoted thing



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
(use-package company)
(use-package company-ghc
  :config
  (add-to-list 'company-backends 'company-ghc))
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))
(use-package smart-mode-line)
(use-package idris-mode
  :bind (:map idris-repl-mode-map
              ("C-c C-k" . idris-repl-clear-buffer))
  )
(use-package helm-idris)
(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (sp-with-modes 'markdown-mode
    (sp-local-pair "```" "```")
    (sp-local-pair "*" "*")
    (sp-local-pair "_" "_")
    (sp-local-pair "⟦" "⟧")
    )
  :bind (:map smartparens-mode-map
              ("C-c u w" . sp-unwrap-sexp))
  )



;; Whitespace-related
(setq whitespace-style '(face tabs lines-tail empty trailing))
(use-package whitespace-cleanup-mode
  :config (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package hydra)
(use-package beacon
  :init
  (beacon-mode 1)
  (setq beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-point-moves-vertically nil
        beacon-blink-when-window-scrolls 0))

(use-package my-kaolin-theme
  :ensure f
  :load-path "lisp/")
(use-package my-split-window
  :ensure f
  :load-path "lisp/"
  :config
  (setq split-window-preferred-function 'my-split-window-sensibly))

(use-package pdf-tools)
(use-package nav-flash)
(use-package fill-column-indicator)
(use-package rainbow-mode)
(use-package centered-window-mode)
(use-package math-symbol-lists
  :config
  (quail-define-package "math" "UTF-8" "Ω" t)
  (quail-define-rules ; add whatever extra rules you want to define here...
   ("->"      "→")
   ("\\bolt"  "⭍") ; Note: This is not covered by Everson Mono
   ("\\from"  #X2190)
   ("\\to"    #X2192)
   ("\\lhd"   #X22B2)
   ("\\rhd"   #X22B3)
   ("\\unlhd" #X22B4)
   ("\\unrhd" #X22B5)
   ("\\ltE"   "Ɛ") ; latin capital letter open E
   ("\\lthkD" "Ɗ") ; latin capital letter D with hook
   ("\\lthkP" "Ƥ") ; latin capital letter P with hook
   ("\\lthkV" "Ʋ") ; latin capital letter V with hook
   ("\\lthkB" "Ɓ") ; latin capital letter B with hook
   ("\\lthkK" "Ƙ") ; latin capital letter K with hook
   ("\\hdots" "…") ; better than \cdots (not midline)
   ("^("      "⁽")
   ("^)"      "⁾")
   ("^+"      "⁺")
   ("^-"      "⁻")
   ("^0"      "⁰")
   ("^1"      "¹")
   ("^2"      "²")
   ("^3"      "³")
   ("^4"      "⁴")
   ("^5"      "⁵")
   ("^6"      "⁶")
   ("^7"      "⁷")
   ("^8"      "⁸")
   ("^9"      "⁹")
   ("^="      "⁼")
   ("^A"      "ᴬ")
   ("^B"      "ᴮ")
   ("^D"      "ᴰ")
   ("^E"      "ᴱ")
   ("^G"      "ᴳ")
   ("^H"      "ᴴ")
   ("^I"      "ᴵ")
   ("^J"      "ᴶ")
   ("^K"      "ᴷ")
   ("^L"      "ᴸ")
   ("^M"      "ᴹ")
   ("^N"      "ᴺ")
   ("^O"      "ᴼ")
   ("^P"      "ᴾ")
   ("^R"      "ᴿ")
   ("^T"      "ᵀ")
   ("^U"      "ᵁ")
   ("^V"      "ⱽ")
   ("^W"      "ᵂ")
   ("^a"      "ᵃ")
   ("^b"      "ᵇ")
   ("^c"      "ᶜ")
   ("^d"      "ᵈ")
   ("^e"      "ᵉ")
   ("^f"      "ᶠ")
   ("^g"      "ᵍ")
   ("^h"      "ʰ")
   ("^i"      "ⁱ")
   ("^j"      "ʲ")
   ("^k"      "ᵏ")
   ("^l"      "ˡ")
   ("^m"      "ᵐ")
   ("^n"      "ⁿ")
   ("^o"      "ᵒ")
   ("^p"      "ᵖ")
   ("^r"      "ʳ")
   ("^s"      "ˢ")
   ("^t"      "ᵗ")
   ("^u"      "ᵘ")
   ("^v"      "ᵛ")
   ("^w"      "ʷ")
   ("^x"      "ˣ")
   ("^y"      "ʸ")
   ("^z"      "ᶻ")
   ("_("      "₍")
   ("_)"      "₎")
   ("_+"      "₊")
   ("_-"      "₋")
   ("_0"      "₀")
   ("_1"      "₁")
   ("_2"      "₂")
   ("_3"      "₃")
   ("_4"      "₄")
   ("_5"      "₅")
   ("_6"      "₆")
   ("_7"      "₇")
   ("_8"      "₈")
   ("_9"      "₉")
   ("_="      "₌")
   ("_a"      "ₐ")
   ("_e"      "ₑ")
   ("_h"      "ₕ")
   ("_i"      "ᵢ")
   ("_j"      "ⱼ")
   ("_k"      "ₖ")
   ("_l"      "ₗ")
   ("_m"      "ₘ")
   ("_n"      "ₙ")
   ("_o"      "ₒ")
   ("_p"      "ₚ")
   ("_r"      "ᵣ")
   ("_s"      "ₛ")
   ("_t"      "ₜ")
   ("_u"      "ᵤ")
   ("_v"      "ᵥ")
   ("_x"      "ₓ")
   )
  (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
        (append math-symbol-list-basic math-symbol-list-extended))
  )

(use-package racket-mode)
(use-package markdown-mode+)
(use-package pandoc-mode)
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  
  
  )

(use-package smooth-scrolling :init (smooth-scrolling-mode))

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
         ("C-x C-b"   . helm-buffers-list)
         ("C-x b"     . helm-buffers-list)
         ("C-x C-f"   . helm-find-files)
         ("M-y"       . helm-show-kill-ring)
         ("M-x"       . helm-M-x)
         ("C-x c o"   . helm-occur)
         ("C-x c s"   . helm-swoop)
         ("C-x c y"   . helm-yas-complete)
         ("C-x c Y"   . helm-yas-create-snippet-on-region)
         ("C-x c b"   . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)
         :map helm-map
         ("<tab>"     . helm-execute-persistent-action)
         ("C-i"       . helm-execute-persistent-action)
         ("C-z"       . helm-select-action)
         )
  )

(use-package helm-ag)
(use-package helm-ghc)
(use-package helm-unicode)

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))


;; Help emacs print Unicode stuff?
(setq ps-multibyte-buffer :bdf-font-except-latin)
(setq bdf-directory-list "/usr/share/emacs/fonts/bdf")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-color "navajo white")
 '(custom-safe-themes
   (quote
    ("88b3e618978518e7117518706043cd68b55eaab0059e6e0528cf876f4ca0acd6" "a7f650df529ed4ab5800925efcfa7361579a2497161a024a470bd5080cf126d3" "aa4dbd81f4771c8589613b327ac85d5ae33b2c3b3056e17064da67f9204a6c18" "efde9eaaa10eea5403ab7f52e01f19bd81d0e8f3bc6edbc32033d9f0b34b329d" "04dc5f91125f3af369c7e13ed4ade69939309fa25f0adb4147e4ea3f168b368c" "b4eb2237584b265184a39bdd44e01d24ff1dd2e31fb2536c047e7b2ec44bbd9c" "a34e11dca3fc94c9d6e5f4ebbef222e060884eb6187a56286d36973b9c0f3776" default)))
 '(package-selected-packages
   (quote
    (math-symbol-lists helm-idris idris-mode whitespace-cleanup-mode racket-mode helm-unicode haskell-snippets yasnippet pandoc exec-path-from-shell beacon f company-ghc rainbow-mode kurecolor fill-column-indicator nav-flash pdf-tools centered-window-mode gitignore-mode undo-tree kaolin-theme rainbow-delimiters smartparens use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
