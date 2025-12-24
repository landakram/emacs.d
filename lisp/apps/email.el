;;; apps/email.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:


  (setq mm-sign-option 'guided)

  (use-package mu4e
    :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
    :commands (mu4e)
    :after general
    :config
    ;; Sync every 10 minutes
    (setq mu4e-update-interval (* 60 10))
    (setq mu4e-maildir (expand-file-name "~/Maildir"))
    (setq mu4e-compose-format-flowed t)

    ;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
    ;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    ;; (setq mu4e-trash-folder  "/[Gmail].Trash")

    ;; don't save message to Sent Messages, GMail/IMAP will take care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    ;; (setq mu4e-maildir-shortcuts
    ;;       '(("/INBOX"             . ?i)
    ;;         ("/[Gmail].Sent Mail" . ?s)
    ;;         ("/[Gmail].Trash"     . ?t)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "mbsync -a")

    (setq mu4e-change-filenames-when-moving t)

    ;; Show images
    (setq mu4e-view-show-images t)

    ;; Don't use mu4e's default HTML renderer. It's hard to read for most messages.
    (setq mu4e-html2text-command "html2text -utf8 -nobs -width 72")
    ;; Ignore mu4e's plaintext heuristic.
    ;; See https://200ok.ch/posts/2018-10-25_disable_mu4e_html_over_plain_text_heuristic.html
    (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

    (setq mu4e-view-show-addresses t)

    (setq browse-url-generic-program 'browse-url-default-browser)

    ;; 
    (add-to-list 'mu4e-view-actions
                 '("open URL" . mu4e-view-go-to-url) t)

    (add-to-list 'mu4e-view-actions
                 '("browser (open in)" . mu4e-action-view-in-browser) t)

    (general-define-key :keymaps '(mu4e-view-mode-map)
                        "J" 'mu4e-view-headers-next
                        "K" 'mu4e-view-headers-prev)

    (setq
     user-mail-address "me@markhudnall.com"
     user-full-name  "Mark Hudnall"
     ;; message-signature
     ;;  (concat
     ;;    "Foo X. Bar\n"
     ;;    "http://www.example.com\n")
     )

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

    (use-package smtpmail
      :ensure t
      :config
      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials
            '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials
            (expand-file-name "~/.authinfo.gpg")
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587
            smtpmail-debug-info t))

    (use-package org-mu4e
      :config)

    (setq mu4e-tags '(
                      "jobs"
                      ))

    (add-to-list 'mu4e-marks
                 '(tagarchive
                   :char       "t"
                   :prompt     "tagarchive"
                   :ask-target (lambda () (completing-read "Choose a tag: " mu4e-tags))
                   :action      (lambda (docid msg target)
                                  (mu4e-action-retag-message msg (concat "+" target ",-\\Inbox"))
                                  (mu4e~proc-move docid mu4e-refile-folder))))

    (add-to-list 'mu4e-marks
                 '(tag
                   :char       "T"
                   :prompt     "tag"
                   :ask-target (lambda () (completing-read "Choose a tag: " mu4e-tags))
                   :action      (lambda (docid msg target)
                                  (mu4e-action-retag-message msg (concat "+" target)))))


    (defun mu4e-headers-mark-for-tag (args)
      "Mark header at point with tag."
      (interactive "P")
      (if args
          (mu4e-headers-mark-and-next 'tag)
          (mu4e-headers-mark-and-next 'tagarchive)))
    (general-define-key :keymaps '(mu4e-headers-mode-map)
                        "t" 'mu4e-headers-mark-for-tag)

    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "Personal"
               :enter-func (lambda () (mu4e-message "Switch to the Personal context"))
               ;; leave-func not defined
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "me@markhudnall.com")))
               :vars '((user-mail-address        . "me@markhudnall.com"  )
                       (user-full-name           . "Mark Hudnall" )
                       (mu4e-compose-reply-to-address           . "me@markhudnall.com" )
                       (mu4e-drafts-folder       . "/Personal/[Gmail]/.Drafts")
                       (mu4e-sent-folder         . "/Personal/[Gmail]/.Sent Mail")
                       (mu4e-trash-folder        . "/Personal/[Gmail]/.Trash")
                       (mu4e-refile-folder       . "/Personal/[Gmail]/.All Mail")
                       (mu4e-maildir-shortcuts   . (("/Personal/INBOX" . ?i)
                                                    ("/Personal/[Gmail]/.Sent Mail"  . ?s)
                                                    ("/Personal/[Gmail]/.All Mail"  . ?a)
                                                    ("/Personal/[Gmail]/.Trash" . ?t)))
                       (mu4e-compose-signature   . nil)))))

    ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
    ;; guess or ask the correct context, e.g.

    ;; start with the first (default) context; 
    ;; default is to ask-if-none (ask when there's no context yet, and none match)
    ;; (setq mu4e-context-policy 'pick-first)

    ;; compose with the current context is no context matches;
    ;; default is to ask 
    ;; '(setq mu4e-compose-context-policy nil)

    (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
    (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
)

(provide 'apps/email)
;;; apps/email.el ends here
