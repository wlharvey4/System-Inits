;;; ~/.gnus.el -*- mode: elisp; -*-
;;; Time-stamp: <2023-11-22 21:09:13 minilolh>

(setq user-full-name     "W. Lincoln Harvey"
      user-mail-address  "lincoln@ccvlp.org")

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-servers-requiring-authorization "smtp.gmail.com")

(setq gnus-extra-headers
      '(X-GM-LABELS))

(setq gnus-select-method
      '(nnmaildir "Mail"
                  (directory "~/.local/share/Mail/")))
