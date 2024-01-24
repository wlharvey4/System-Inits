;;; init-local.el --- Local Lisp support -*- lexical-binding: t -*-
;;; Time-stamp: <2024-01-23 17:06:14 minilolh>
;;; Commentary:
;;; Code:


;; Add the Super and Hyper modifer keys to Mac
(setq mac-right-option-modifier 'super)
(setq ns-function-modifier 'hyper)

;;(setq org-agenda-include-diary t)

(add-hook 'before-save-hook 'time-stamp t)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(require 'ox-texinfo)
(require 'ob-http)

(add-to-list 'Info-directory-list "~/.local/share/share/info/")
(add-to-list 'Info-directory-list "~/.local/share/denote")
;;;(add-to-list 'Info-directory-list "/opt/local/share/info/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org-Mode

(setq org-attach-method 'lns)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "C-c C-."
                        'org-time-stamp-inactive)))

(setq org-todo-keywords
      '((sequence "TODO(t@)" "NEXT(n)" "WAIT(w@)" "HOLD(h@)" "|" "DONE(d!)" "RECEIVED(!)" "CANCELLED(c!)" )
        (sequence "DRAFT(D@)" "DRAFTING(!)" "|" "DRAFTED(!)")
        (sequence "LETTER(l@)" "|" "WROTE(!)")
        (sequence "REQUEST(r)" "|" "REQUESTED(R!)")
        (sequence "DELEGATE(@)" "CHECK(@)" "|" "DELEGATED(!)")
        (sequence "TASK(T!)" "|" "COMPLETED(C!)")))

(setq org-agenda-files
      '("~/.local/share/notes/ccvlp2/cases"
        "~/.local/share/notes/personal"
        "~/.local/share/notes/law"
        "~/.local/share/notes/legal"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
       ;; (make-mu4e-context
       ;;  :name "LOLH"
       ;;  :match-func
       ;;  (lambda (msg)
       ;;    (when msg (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
       ;;  :vars
       ;;  '((user-mail-address  . "lincolnlaw@mac.com")
       ;;    (user-full-name     . "W. Lincoln Harvey")
       ;;    (mu4e-refile-folder . "/icloud/Archive")
       ;;    (mu4e-sent-folder   . "/icloud/Sent Messages")
       ;;    (mu4e-drafts-folder . "/icloud/Drafts")
       ;;    (mu4e-trash-folder  . "/icloud/Deleted Messages")
       ;;    (mu4e-sent-messages-behavior . sent)
       ;;                                  ; add a signature
       ;;                                  ; (mu4e-compose-signature . "...")
       ;;    (smtpmail-smtp-server . "smtp.mail.me.com")))
       ))

;;; mu4e - shortcuts to the folders; show up in the mode line
(setq mu4e-maildir-shortcuts
      '((:maildir "/gmail/Inbox"                :key ?i)
        (:maildir "/gmail/[Gmail]/All Mail"     :key ?a)
        (:maildir "/gmail/[Gmail]/Sent Mail"    :key ?s)
        (:maildir "/gmail/[Gmail]/Trash"        :key ?t)
        (:maildir "/gmail/[Gmail]/Drafts"       :key ?d)))

;;; smtpmail - config
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      ;;      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtp-debut-info t
      message-kill-buffer-on-exit t)

(define-key global-map (kbd "C-c n m") #'mu4e-org-store-and-capture)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom functions
(defun init-emacs ()
  "Open the init.el file in a new frame for editing."
  (interactive)
  (find-file "~/.config/emacs/lisp/init-local.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Denote
(setq denote-directory "/Users/minilolh/.local/share/notes")
(setq denote-dired-directories
      (list
       denote-directory
       (expand-file-name "~/.local/share/notes/ccvlp2")
       (expand-file-name "~/.local/share/notes/ccvlp2/cases")
       (expand-file-name "~/.local/share/notes/ccvlp2/cases/clients")
       (expand-file-name "~/.local/share/notes/ccvlp2/cases/attorneys")
       (expand-file-name "~/.local/share/notes/ccvlp2/cases/closed")
       (expand-file-name "~/.local/share/notes/legal")
       (expand-file-name "~/.local/share/notes/personal")))
(setq denote-dired-directories-include-subdirectories t)

(diredfl-global-mode -1) ; dired-mode does not work with diredfl
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(setq denote-prompts '(title keywords signature))
(setq denote-date-prompt-use-org-read-date t)

(require 'denote-org-dblock)
(require 'denote-silo-extras)
(require 'template-funcs)

(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-add-links)
  (define-key map (kbd "C-c n b") #'denote-backlinks)
  (define-key map (kbd "C-c n f f") #'denote-find-link)
  (define-key map (kbd "C-c n f b") #'denote-find-backlink)
  (define-key map (kbd "C-c n f s") #'denote-sort-dired)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter)
  (define-key map (kbd "C-c n l") #'denote-link-after-creating)
  (define-key map (kbd "C-c n L") #'denote-link-or-create)
  (define-key map (kbd "C-c n C") #'denote-silo-extras-create-note) ; Create
  (define-key map (kbd "C-c n O") #'denote-silo-extras-open-or-create) ; Open-or-Create
  (define-key map (kbd "C-c n S") #'denote-silo-extras-select-silo-then-command)) ; Select-then-Command

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
  (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

(setq denote-silo-extras-directories
      '("~/.local/share/notes/ccvlp2"
        "~/.local/share/notes/ccvlp2/cases"
        "~/.local/share/notes/ccvlp2/cases/clients"
        "~/.local/share/notes/ccvlp2/closed"
        "~/.local/share/notes/law"
        "~/.local/share/notes/legal"
        "~/.local/share/notes/personal"))

;; denote-silo-extras-create-note  :: prompts  for  a directory  among
;; denote-silo-extras-directories  and runs  the  denote command  from
;; there.
;;
;; denote-silo-extras-open-or-create :: prompts  for a directory among
;; denote-silo-extras-directories  and runs  the denote-open-or-create
;; command from there.
;;
;; denote-silo-extras-select-silo-then-command    ::   prompts    with
;; minibuffer      completion      for     a      directory      among
;; denote-silo-extras-directories.  Once  the user  selects a  silo, a
;; second prompt asks for a  Denote note-creation command to call from
;; inside that silo.

;;; (setq denote-link-backlinks-display-buffer-action
;;       '((display-buffer-reuse-window
;;          display-buffer-in-side-window)
;;         (side . left)
;;         (slot . 99)
;;         (window-width . 0.3)))

(setq denote-templates
      `((tinyurl . ,(tinyurl))
        (client . ,(newclient))
        (newcase . ,(newcase))))

(setq diary-file "~/.local/share/diary")
(diary)

(provide 'init-local)
;;; init-local.el ends here
