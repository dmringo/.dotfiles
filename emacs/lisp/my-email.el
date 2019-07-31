;; TODO: should I be using use-package here or in init.el?

;; This is almost certainly where mu4e will be installed, but it feels awfully
;; ugly
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'smtpmail)
(require 'message)
(require 'alert)

(use-package mu4e-alert
  :config
  (mu4e-alert-set-default-style 'log)
  (add-hook 'after-init-hook
            (defun my/mu4e-enable-alerts ()
              (mu4e-alert-enable-notifications)
              (mu4e-alert-enable-mode-line-display))))


(setq
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
 ;; This has the effect of setting the point below the cited email in a message
 ;; reply.  For me, at least, bottom or inline posting style is preferable, but
 ;; 'traditional (which is nominally "inline") places the point as if for
 ;; top-posting.
 message-cite-reply-position 'below
 ;; Bit of a hack using the internal variable here, but I'd really like to be
 ;; able to use loopback pinentry with gpg (invoked by mbsync)
 mu4e~get-mail-password-regexp (concat "^Enter passphrase: $|" mu4e~get-mail-password-regexp)
 )

(setq old-mu4e-pwd-regex mu4e~get-mail-password-regexp)
(setq mu4e~get-mail-password-regexp (concat "^Enter passphrase: $\\|" old-mu4e-pwd-regex))

(defun my/mu4e-set-pandoc-fmt (fmt)
  (interactive (list (completing-read
                      "Select format: "
                      (split-string
                       ;; could work with internal var pandoc--formats from
                       ;; pandoc mode, but that feels wrong
                       (shell-command-to-string "pandoc --list-output-formats"))
                      nil 'require-match nil 'my/pandoc-fmt-hist)))
  ;; It would be better to verify that its a real format, but this is better
  ;; than nothing
  (setq fmt (or fmt "plain"))
  (let* ((old-cmd mu4e-html2text-command)
         (pandoc-ifmt "html-native_spans-native_divs")
         (pandoc-ofmt (shell-quote-argument fmt))
         ;; needed so pandoc doesn't choke on invalid bytes
         (iconv-opts "-c -t UTF-8")
         (cmd (format "iconv %s | pandoc -f %s -t %s"
                      iconv-opts pandoc-ifmt pandoc-ofmt)))
    ;; If nothing has changed, no need to set the mu4e var or refresh the display
    (unless (equal old-cmd cmd)
      (setq mu4e-html2text-command cmd)
      (when-let ((win (get-buffer-window mu4e~view-buffer-name)))
        (select-window win)
        (mu4e-view-refresh)))))

(bind-keys :map mu4e-view-mode-map
           ("c" . my/mu4e-set-pandoc-fmt)
           ("G" . mu4e-view-refresh))

(my/mu4e-set-pandoc-fmt "plain")



;; use imagemagick for displaying images
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Add an action to view a message as HTML in the browser
(add-to-list 'mu4e-view-actions
             (cons "View in browser" 'mu4e-action-view-in-browser) t)

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook
 'mu4e-headers-mode-hook
 (defun my/mu4e-change-headers ()
   (interactive)
   (setq mu4e-headers-fields
         `((:human-date . 25) ;; alternatively, use :date
           (:flags . 6)
           (:from . 22)
           (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
           (:size . 7)))))


(add-hook
 'mu4e-headers-search-hook
 (defun my/mu4e-switch-context-on-search (query)
   "Trys to switch the `mu4e' context intelligently by
checking for \"maildir:\" in a search query.  If the first path
element of the maildir in the query is not a context, this may
blow up."
   (if-let*
       ((_ (string-match "maildir:\\\"/\\([^/]+\\)/.*\\\"" query))
        (ctx (match-string 1 query)))
       (mu4e-context-switch nil ctx))))

(add-hook
 'mu4e-compose-mode-hook
 (defun my/mu4e-setup-compose ()
   "Setup function for composing email from `mu4e'"
   (flyspell-mode)))

(add-hook
 'mu4e-view-mode-hook
 (defun my/mu4e-setup-view ()
   "Setup function for viewing email from `mu4e'"
   (turn-on-visual-line-mode)))

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


(defvar my/sendmail-prog "sendmail.sh"
  "Program to use for `my/sendmail'")

(defvar my/sendmail-args nil
  "Arguments to be given to `my/sendmail-prog' before any
  others")

(defvar my/sendmail-debug t
  "Whether to log debugging output.  If set to a string, it is
  interpreted as a buffer name to log to.  Otherwise, if non-nil,
  logging is done with `message'")

(defvar my/sendmail-buf-pfx "my/sendmail"
  "Buffer name prefix for my/sendmail processes")

(defun my/sendmail-get-buf (proc stream)
  "Get the name of the buffer for PROC's STREAM
STREAM must be one of 'stdout  or 'stderr"
  (or (memq stream '(stdout stderr))
      (signal 'wrong-type-argument
              `(memq ,stream (stdout stderr))))
  (format "*%s (%s) %s*"
          my/sendmail-buf-pfx (process-id proc) stream))

(defun my/sendmail-sentinel (proc msg)
  "Sentinel for processed created by `my/sendail'.
When `my/sendmail-debug' is non-nil, process state changes are
logged by this function.  On unexpected failure, alerts are
created to show what buffers my have more information"
  (let* ((status (process-status proc))
         (proc-pid (process-id proc))
         (name (process-name proc)))
    (pcase status
      ('exit ;; for a process that has exited.
       (let ((code (process-exit-status proc)))
         (unless (zerop code)
           (alert (format "Sendmail exited with code %s\nSee buffers %s and/or %s"
                          code
                          (my/sendmail-get-buf proc 'stdout)
                          (my/sendmail-get-buf proc 'stderr))
                  :severity 'high))))
      ('signal ;; for a process that has got a fatal signal.
       (alert (format "Sendmail process got fatal signal: %s\nSee buffers %s and/or %s"
                      msg
                      (my/sendmail-get-buf proc 'stdout)
                      (my/sendmail-get-buf proc 'stderr))
              :severity 'high)))
    (when my/sendmail-debug
      (let ((log-msg (format "Process %s changed status to %s: %s"
                          name status msg)))
        (if (stringp my/sendmail-debug)
            (with-current-buffer my/sendmail-debug
              (insert log-msg) (newline))
          (message log-msg))))))


;; WIP
(defun my/sendmail ()
  "My custom sendmail function using `my/sendmail-prog'.
Similar to `message-send-mail-with-sendmail' but simplified for
use with my msmtp wrapper \"sendmail.sh\". Arguments are solely determined by
`my/sendmail-args'"
  (require 'message)
  (let ((case-fold-search t))
    (save-restriction
      (message-narrow-to-headers))
    ;; Change header-delimiter to be what sendmail expects.
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (replace-match "\n")

    (when message-interactive
      (with-current-buffer errbuf
        (erase-buffer))))
  (when-let*
      (;; (default-directory "/")  - Don't know if I need this...
       (prog-with-args (cons my/sendmail-prog my/sendmail-args))
       (tmp-out-buf "*sendmail-tmp-out*")
       (tmp-err-buf "*sendmail-tmp-err*")
       ;; Even if the command doesn't exist, this still creates a process object
       ;; (to say nothing about when the command exits immediately)
       (proc (condition-case err-var
                 (make-process
                  :name "my/sendmail process"
                  :buffer tmp-out-buf
                  :stderr tmp-err-buff
                  :coding message-send-coding-system
                  :command prog-with-args)
               (error
                (destructuring-bind (_ . data) err-var
                  (message "Failed to make sendmail process")
                  ;; dolist will yield nil and break out of the `when-let'
                  (dolist (reason data) (message "  %s" reason))))))
       (proc-pid (process-id proc)))
    ;; This yields fairly reliably unique buffer names for separate sendmail
    ;; processes.  I don't imagine there will ever be more than a few such
    ;; processes running, so this is probably overkill.
    (with-current-buffer tmp-out-buf (rename-buffer
                                      (format "*%s: %s*" my/sendmail-stdout-buf proc-pid)))
    (with-current-buffer tmp-err-buf (rename-buffer
                                      (format "*%s: %s*" my/sendmail-stderr-buf proc-pid)))
    (process-send-region proc (point-min) (point-max))))




;; TODO: Use msmtp and `message-send-mail-with-sendmail'.

;; Probably can configure args to msmtp in the mu4e contexts, but it may be
;; better to just use a properly configured ~/.msmtprc
(cl-defmacro my/make-mu4e-context (name email smtp-spec &key match-fn vars)
  "wrapper for `make-mu4e-context' specific to my setup.
This references `mu4e-maildir', so make sure that's set."
  `(make-mu4e-context
    :name ,name
    :enter-func ,(lambda () (mu4e-message "Entering context: [%s]" name))
    :leave-func ,(lambda () (mu4e-message "Leaving context: [%s]" name))
    :match-func
    ,(or match-fn
         `(lambda (msg)
            (and msg
                 (mu4e-message-contact-field-matches
                  msg '(:to :cc :bcc) ,email))))
    :vars
    (quote
     ((user-mail-address          . ,email)
      (mu4e-sent-folder           . ,(f-join "/" name "sent"))
      (mu4e-drafts-folder         . ,(f-join "/" name "drafts"))
      (mu4e-trash-folder          . ,(f-join "/" name "trash"))
      (mu4e-refile-folder         . ,(f-join "/" name "archive"))
      (smtpmail-queue-dir         . ,(f-join mu4e-maildir name "q"))
      (send-mail-function         . sendmail-send-it)
      (message-send-mail-function . sendmail-send-it)
      (smtpmail-debug-info        . t)
      (smtpmail-debug-verbose     . t)
      (mu4e-get-mail-command      . ,(format "mbsync %s" name))
      ;; WIP
      (my/sendmail-prog           . "smailq")
      ;; split the smtp-spec as host:port.  Anything else is an error
      ,(pcase (split-string smtp-spec ":")
         (`(,host ,port)
            (cons 'my/sendmail-args
                  (list name my/location host port)))
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
        "mail.lanl.gov:25")))

;; shortcut
(mu4e-bookmark-define
 "to:dringo@lanl.gov AND maildir:/inbox"
 "LANL inbox only"
 ?l)


(provide 'my-email)
