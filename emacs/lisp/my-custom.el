(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [("#181818" . "#282828")
    ("#ab4642" . "#dc9656")
    ("#a1b56c" . "#383838")
    ("#f7ca88" . "#383838")
    ("#7cafc2" . "#585858")
    ("#ba8baf" . "#b8b8b8")
    ("#86c1b9" . "#d8d8d8")
    ("#ffffff" . "#ffffff")])
 '(cursor-type (quote box))
 '(custom-safe-themes
   (quote
    ("d8cdb4d217de24b42a10da320f60f51ff72179469c706032d4bed032dd9b7bd4" "43e61abee8a9720b2c3be5c6c0fe90c87b55ecf8fba4c38a33d89d023dffa7f7" "b948ebd1b7a3a37db6b931dc122ebb8889d2b0d32fcfe93005745128dc904244" "4bfced46dcfc40c45b076a1758ca106a947b1b6a6ff79a3281f3accacfb3243c" "04317336f6010f89e444b5ffc5b5a273290cd4b24a21bc13a5785179d7998a26" "8d55185d74000bc7caaea2105f4b8410cd0f82e1148e7ba93e2bcdcea275dbbf" "3c9f63378e7f1d64452d063b3475c8c0e7d8726b0c681c1e3943b2c605a26bed" "6fa6524f51574b984e239e203b59b2f81da1a1b282c2ac09bba8e6198892c924" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "88b3e618978518e7117518706043cd68b55eaab0059e6e0528cf876f4ca0acd6" default)))
 '(fci-rule-color "#222222")
 '(package-selected-packages
   (quote
    (eziam-theme org-plus-contrib elm-mode origami eimp ein anaconda-mode conda nov py-autopep8 elpy atomic-chrome counsel-etags helm-projectile projectile-ripgrep projectile flycheck-rtags company-quickhelp flycheck-irony company-irony irony-eldoc irony rtags cmake-ide cmake-mode company-c-headers helm-gtags treemacs which-key keyfreq pandoc-mode mmm-mode dokuwiki-mode alect-themes basic-theme helm-tramp my-utils edit-indirect math-symbol-lists helm-idris idris-mode whitespace-cleanup-mode racket-mode helm-unicode haskell-snippets yasnippet pandoc exec-path-from-shell beacon f company-ghc rainbow-mode kurecolor fill-column-indicator nav-flash pdf-tools centered-window-mode gitignore-mode undo-tree rainbow-delimiters smartparens use-package)))
 '(safe-local-variable-values
   (quote
    ((eval local-set-key
           (kbd "C-c C-k")
           (lambda nil
             (interactive)
             (compile "make -k")))
     (flycheck-clang-language-standard "c++11")
     (flycheck-clang-language-standard "c++-11")
     (intero-targets "cs558:lib" "cs558:test:spec"))))
 '(show-paren-mode t)
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ba8baf")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858")
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Everson Mono"))))
 '(fringe ((t (:background "#1b1b1b")))))
