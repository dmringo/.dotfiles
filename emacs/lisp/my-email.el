



;; TODO: should I be using use-package here? Or maybe `require'?
(use-package org-mime)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)

(setq
 ;; TODO: set this per-host maybe?
 ;;  
 ;; It would be nice if this was parameterized on a context or something
 mu4e-get-mail-command "mbsync -a"
 ;; Keep downloads consistent with other apps
 mu4e-attachment-dir "~/Downloads"
 ;; TODO: Think about this one...
 mu4e-view-prefer-html t
 ;; Don't update mailboxes automatically (default)
 mu4e-update-interval nil
 ;; Update headers if indexing changes them (default)
 mu4e-headers-auto-update t
 ;; I don't like signatures atm
 mu4e-compose-signature-auto-include nil
 ;; TODO: Experiment with this one
 ;; It seems reasonable though
 mu4e-compose-format-flowed t
 ;; enable inline images
 mu4e-view-show-images t
 ;; Don't save message - let IMAP do it
 mu4e-sent-messages-behavior 'delete
 ;;
 )
;; use imagemagick for displaying images
(when (fbounp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Add an action to view a message as HTML in the browser
(add-to-list 'mu4e-view-actions
             (cons "View in browser" 'mu4e-action-view-in-browser) t)



