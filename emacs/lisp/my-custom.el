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
    ("3d20cf0dbc6465a02c468abf2d9b8c17e88b20fbc05a04205a829285da28799d" "4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "a866134130e4393c0cad0b4f1a5b0dd580584d9cf921617eee3fd54b6f09ac37" "2a1b4531f353ec68f2afd51b396375ac2547c078d035f51242ba907ad8ca19da" "7666b079fc1493b74c1f0c5e6857f3cf0389696f2d9b8791c892c696ab4a9b64" "53d1bb57dadafbdebb5fbd1a57c2d53d2b4db617f3e0e05849e78a4f78df3a1b" "2af26301bded15f5f9111d3a161b6bfb3f4b93ec34ffa95e42815396da9cb560" "77c3f5f5acaa5a276ca709ff82cce9b303f49d383415f740ba8bcc76570718b9" "0846e3b976425f142137352e87dd6ac1c0a1980bb70f81bfcf4a54177f1ab495" "b5ecb5523d1a1e119dfed036e7921b4ba00ef95ac408b51d0cd1ca74870aeb14" "d8cdb4d217de24b42a10da320f60f51ff72179469c706032d4bed032dd9b7bd4" "43e61abee8a9720b2c3be5c6c0fe90c87b55ecf8fba4c38a33d89d023dffa7f7" "b948ebd1b7a3a37db6b931dc122ebb8889d2b0d32fcfe93005745128dc904244" "4bfced46dcfc40c45b076a1758ca106a947b1b6a6ff79a3281f3accacfb3243c" "04317336f6010f89e444b5ffc5b5a273290cd4b24a21bc13a5785179d7998a26" "8d55185d74000bc7caaea2105f4b8410cd0f82e1148e7ba93e2bcdcea275dbbf" "3c9f63378e7f1d64452d063b3475c8c0e7d8726b0c681c1e3943b2c605a26bed" "6fa6524f51574b984e239e203b59b2f81da1a1b282c2ac09bba8e6198892c924" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "88b3e618978518e7117518706043cd68b55eaab0059e6e0528cf876f4ca0acd6" default)))
 '(fci-rule-color "#222222")
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(package-selected-packages
   (quote
    (rjsx-mode doom-themes eziam-theme org-plus-contrib elm-mode origami eimp ein anaconda-mode conda nov py-autopep8 elpy atomic-chrome counsel-etags helm-projectile projectile-ripgrep projectile flycheck-rtags company-quickhelp flycheck-irony company-irony irony-eldoc irony rtags cmake-ide cmake-mode company-c-headers helm-gtags treemacs which-key keyfreq pandoc-mode mmm-mode dokuwiki-mode alect-themes basic-theme helm-tramp my-utils edit-indirect math-symbol-lists helm-idris idris-mode whitespace-cleanup-mode racket-mode helm-unicode haskell-snippets yasnippet pandoc exec-path-from-shell beacon f company-ghc rainbow-mode kurecolor fill-column-indicator nav-flash pdf-tools centered-window-mode gitignore-mode undo-tree rainbow-delimiters smartparens use-package)))
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
