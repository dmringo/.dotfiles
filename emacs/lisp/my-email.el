;; TODO: should I be using use-package here or in init.el?
(use-package org-mime)

;; This is almost certainly where mu4e will be installed, but it feels awfully
;; ugly
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'smtpmail)


(setq
 ;; TODO: set this per-host maybe?
 ;; It would be nice if this was parameterized on a context or something
 mu4e-get-mail-command "mbsync -a"
 ;; Keep downloads consistent with other apps
 mu4e-attachment-dir "~/Downloads"
 ;; Prefer text over HTML (default)
 mu4e-view-prefer-html nil
 ;; This really forces text-view (see doc). Toggle with h in message view
 mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
 ;; Don't update mailboxes automatically (default)
 mu4e-update-interval nil
 ;; Update headers if indexing changes them (default)
 mu4e-headers-auto-update t
 ;; I don't like signatures atm
 mu4e-compose-signature-auto-include nil
 ;; set format=flowed (https://joeclark.org/ffaq.html)
 mu4e-compose-format-flowed t
 ;; enable inline images (only relevant to HTML view, I assume)
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
 ;; my maildir is always here:
 mu4e-maildir "~/mail/"
 ;; use generic `completing-read' since it should be configued to use whatever
 ;; completion system I'm already invested in
 mu4e-completing-read-function 'completing-read
 )
;; use imagemagick for displaying images
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Add an action to view a message as HTML in the browser
(add-to-list 'mu4e-view-actions
             (cons "View in browser" 'mu4e-action-view-in-browser) t)

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
          (defun my/mu4e-change-headers ()
            (interactive)
            (setq mu4e-headers-fields
                  `((:human-date . 25) ;; alternatively, use :date
                    (:flags . 6)
                    (:from . 22)
                    (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                    (:size . 7)))))

(add-hook 'mu4e-headers-search-hook
          (defun my/h-search-hook (query)
            (if-let*
                ((_ (string-match "maildir:\\\"/\\([^/]+\\)/.*\\\"" query))
                 (ctx (match-string 1 query)))
                (mu4e-context-switch nil ctx))))

(add-hook 'mu4e-compose-mode-hook
          (defun my/setup-compose ()
            "Setup function for composing email from `mu4e'"
            (flyspell-mode)))

;; There's probably a better way of doing this, rather than a complete redef.
;; `defadvice' doesn't seem to work so well though, since it's expecting that
;; the advised function will eventually be called.
(defun mu4e~context-ask-user (prompt)
  "Complete override of `mu4e-context-ask-user' for better completion.
This replicates the logic of the function it advises, but calls
`completing-read' rather than relying on unique first characters
of contexts"
  (when mu4e-contexts
    (let* ((names
            (cl-map 'list
                    (lambda (ctx)
                      (cons (mu4e-context-name ctx) ctx))
                    mu4e-contexts))
           (ctx-name (completing-read prompt names))
           (ctx (cdr-safe (assoc ctx-name names))))
      (or ctx (mu4e-error "No such context")))))



(cl-defmacro my/make-mu4e-context (name email smtp-spec &key match-fn vars)
  "wrapper for `make-mu4e-context' specific to my setup.
This references `mu4e-maildir', so make sure that's set."
  `(make-mu4e-context
    :name ,name
    :enter-func ,(lambda () (mu4e-message "Entering context: [%s]" name))
    :leave-func ,(lambda () (mu4e-message "Leaving context: [%s]" name))
    :match-func
    ,(or match-fn
         (lambda (msg)
           (and msg
                (mu4e-message-contact-field-matches
                 msg '(:to :cc :bcc) email))))
    :vars
    (quote
     ((user-mail-address          . ,email)
      (mu4e-sent-folder           . ,(f-join "/" name "sent"))
      (mu4e-drafts-folder         . ,(f-join "/" name "drafts"))
      (mu4e-trash-folder          . ,(f-join "/" name "trash"))
      (smtpmail-queue-dir         . ,(f-join mu4e-maildir name "q"))
      (send-mail-function         . smtpmail-send-it)
      (message-send-mail-function . smtpmail-send-it)
      (smtpmail-debug-info        . t)
      (smtpmail-debug-verbose     . t)
      (mu4e-get-mail-command      . ,(format "mbsync %s" name))
      ;; split the smtp-spec as host:port.  Anything else is an error
      ,@(pcase (split-string smtp-spec ":")
          (`(,host ,port)
           (list (cons 'smtpmail-smtp-server host)
                 (cons 'smtpmail-smtp-service (string-to-number port))))
          (_ (error "Bad `smtp-spec': %S" smtp-spec)))
      ,@vars))))


;; Contexts
(setq mu4e-contexts
      (list
       (my/make-mu4e-context
        "dmr"
        "davidmringo@gmail.com"
        "smtp.gmail.com:587")
       (my/make-mu4e-context
        "lanl"
        "dringo@lanl.gov"
        "mail.lanl.gov:25")
       ))


;; NOTE: Probably need to change mu4e-*-folder vars.
;;
;; With how `mu' and `mu4e' work now, it looks like if you have multiple
;; accounts and maildirs for each (e.g. dmr/inbox and lanl/inbox), jumping to
;; the "/inbox" in `mu4e' will actually show you everything in both inboxes.
;; That may be useful, but my expectation is that the "/inbox" is relative to
;; the `mu4e-maildir' and won't match some other maildir.  Of course, the manual
;; warns that setting this variable has no effect without restarting `mu4e', so
;; maybe

;; shortcut
(mu4e-bookmark-define
 "to:dringo@lanl.gov AND maildir:/inbox"
 "LANL inbox only"
 ?l)
