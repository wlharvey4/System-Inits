;;; init-local.el --- Local Lisp support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; mu/mu4e - Mail User Agent
;;; mu init -m ~/.local/share/mail --my-address me@gmail.com --my-address me@mac.com ...
;;; mu index
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html

(setq mail-user-agent 'mu4e-user-agent)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;;; mu4e - config
(setq mu4e-get-mail-command "mbsync gmail" ; U to update from the mainview
      mu4e-maildir (expand-file-name "~/.local/share/mail")
      mu4e-attachment-dir (expand-file-name "~/Downloads")
      mu4e-compose-format-flowed t
      mu4e-html2text-command "w3m -T text/html" ; there are many options
      mu4e-update-interval 600
      mu4e-index-update-in-background t
      mu4e-headers-auto-update t
      mu4e-change-filenames-when-moving t
      mu4e-context-policy 'pick-first)

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "CCVLP"
        :match-func
        (lambda (msg)
          (when msg (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
        :vars
        '((user-mail-address  . "lincoln@ccvlp.org")
          (user-full-name     . "W. Lincoln Harvey")
          (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
          (mu4e-sent-folder   . "/gmail/[Gmail]/Sent Mail")
          (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
          (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")
          (mu4e-sent-messages-behavior . delete)
                                        ; add a signature
                                        ; (mu4e-compose-signature . "...")
          (smtpmail-smtp-server . "smtp.gmail.com")))
       (make-mu4e-context
        :name "LOLH"
        :match-func
        (lambda (msg)
          (when msg (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
        :vars
        '((user-mail-address  . "lincolnlaw@mac.com")
          (user-full-name     . "W. Lincoln Harvey")
          (mu4e-refile-folder . "/icloud/Archive")
          (mu4e-sent-folder   . "/icloud/Sent Messages")
          (mu4e-drafts-folder . "/icloud/Drafts")
          (mu4e-trash-folder  . "/icloud/Deleted Messages")
          (mu4e-sent-messages-behavior . sent)
                                        ; add a signature
                                        ; (mu4e-compose-signature . "...")
          (smtpmail-smtp-server . "smtp.mail.me.com")))))

;;; mu4e - shortcuts to the folders; show up in the mode line
(setq mu4e-maildir-shortcuts
      '((:maildir "/gmail/Inbox"                :key ?i)
        (:maildir "/gmail/[Gmail]/All Mail"     :key ?a)
        (:maildir "/gmail/[Gmail]/Sent Mail"    :key ?s)
        (:maildir "/gmail/[Gmail]/Trash"        :key ?t)
        (:maildir "/gmail/[Gmail]/Drafts"       :key ?d)
        (:maildir "/icloud/Inbox"               :key ?I)
        (:maildir "/icloud/Sent Messages"       :key ?S)
        (:maildir "/icloud/Deleted Messages"    :key ?T)
        (:maildir "/icloud/Drafts"              :key ?D)))


;;; smtpmail - config
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      ;;      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtp-debut-info t
      message-kill-buffer-on-exit t)


;;; Custom functions
(defun init-emacs ()
  "Open the init.el file in a new frame for editing."
  (interactive)
  (find-file-other-frame user-init-file))


(provide 'init-local)
;;; init-local.el ends here
