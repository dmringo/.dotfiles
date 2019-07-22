



;; TODO: should I be using use-package here? Or maybe `require'?
(use-package org-mime)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)


(setq
 ;; TODO: set this per-host maybe?
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
 ;; Have to set this when using mbsync
 mu4e-change-filenames-when-moving t
 ;; Don't keep buffer around when done with `message'
 message-kill-buffer-on-exit t
 ;; Don't reply to self 0_0
 mu4e-compose-dont-reply-to-self t
 ;; Show full addresses, not just names (toggle with M-<RET>)
 mu4e-view-show-addresses  t
 ;; override the default of ~/Maildir
 mu4e-maildir "~/mail"
 )
;; use imagemagick for displaying images
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Add an action to view a message as HTML in the browser
(add-to-list 'mu4e-view-actions
             (cons "View in browser" 'mu4e-action-view-in-browser) t)


(add-hook 'mu4e-compose-mode-hook
          (defun my/setup-compose ()
            "Setup function for composing email from `mu4e'")
          (flyspell-mode))

(require 'smtpmail)

;; Contexts
(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "dmr"
        :enter-func (lambda () (mu4e-message "Entering dmr context"))
        :leave-func (lambda () (mu4e-message "Entering dmr context"))
        :match-func
        (lambda (msg)
          (and msg
               (mu4e-message-contact-field-matches
                msg '(:from :to :cc :bcc) "davidmringo@gmail.com")))
        :vars
        '((user-mail-address          . "davidmringo@gmail.com")
          (mu4e-sent-folder           . "/dmr/sent")
          (mu4e-drafts-folder         . "/dmr/drafts")
          (mu4e-trash-folder          . "/dmr/trash")
          (smtpmail-queue-dir         . "~/mail/dmr/q")
          (message-send-mail-function . 'smtpmail-send-it)
          (smtpmail-smtp-user         . "google/isync-dmr")
          (smtpmail-smtp-server       . "smtp.gmail.com")
          (smtpmail-smtp-service      . 587)
          (smtpmail-debug-info        . t)
          (smtpmail-debug-verbose     . t)
          (mu4e-get-mail-command      . "mbsync dmr")
          ))))

