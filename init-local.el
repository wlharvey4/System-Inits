;;; init-local.el --- Local Lisp support -*- lexical-binding: t -*-
;;; Time-stamp: <2025-07-09 08:44:29 lolh-mbp-16>

;;; Commentary:
;;; init-local.el

;;; This code covers the following local configurations:
;;; 0. purcell/emacs.d => ~/.local/share/emacs/purcell-emacs.d/
;;;    0.1. ~/.config/emacs is a symlink to purcell-emacs.d/
;;;    0.2 ~/.local/share/emacs/purcell-emacs.d/list
;;; 1. mu/mu4e => ~/.local/share/mu installed into /usr/local/
;;;    1.1. mu is installed in /usr/local/bin
;;;    1.2. mu4e is installed into /usr/local/share/emacs/site-lisp
;;;    1.3./usr/local/share/emacs/site-lisp/mu4e must be added to load-path
;;;    1.4. mu4e.info  is installed into /usr/local/share/info
;;;    1.5. /usr/local/share/info must be added to INFODIR
;;;    1.6. Configure mu4e
;;; 2. Denote => ~/.local/share/emacs/denote/
;;;    2.1. README.org needs to be compiled into denote.info, and installed into dir
;;;    2.2. Add key bindings
;;;    2.3. Set up default denote directory => ~/.local/share/notes
;;;    2.4. Set up silos
;;;         i. ccvlp2
;;;        ii. law
;;;       iii. legal
;;;        iv. personal
;;; 3. Org
;;;    3.1. require ox-texinfo to be able to export to info files
;;;    3.2. org-attach-method needs to be set of lns
;;;    3.3. set org-indent-mode to get rid of multiple stars in headings
;;;    3.4. Add key C-c C-. for an inactive time stamp
;;;    3.5. Add todo keywords
;;;    3.6. Add org agenda files
;;; 4. Diary
;;;    4.1. Set diary file to ~/.local/share/emacs/diary
;;; 5. Emacs
;;;    5.1. time-stamp
;;;    5.2. visual-line-mode
;;;    5.3. dired-hide-details-mode
;;;    5.4. bookmark-default-file => ~/.local/share/emacs/site-list/bookmarks.el
;;;           symlink ~/.local/src/System-Inits/bookmarks.el to
;;;           ~/.local/share/emacs/site-list/bookmarks.el
;;; 6. Local Emacs Code should be Symlinked into a Site Lisp directory
;;;    6.1. ~/.local/src/emacs/utils/template-funcs -> ~/.local/share/emacs/site-lisp/template-funcs
;;;    6.2, ~/.local/src/emacs/utils/extract -> ~/.local/share/emacs/site-lisp/extract
;;; 7. Common Lisp
;;;    7.1 Prefix is ~/.local/src/common-lisp
;;;    7.2 bin is ~/.local/src/common-lisp/bin
;;;    7.3 implementations at ~/.local/source/common-lisp/implementations
;;; Appendix
;;; A. Maximize Screen on Opening: https://www.emacswiki.org/emacs/FullScreen
;;;    - variable `ns-use-native-fullscreen'=t means use native fullscreen
;;;    - see `ns group'
;;;    A.1. Emacs will start at a default frame size (small) and then expand if you maximize it
;;;         To avoid this distracting event, add the  following code to the early-init.el file:
;;;         (push '(fullscreen . maximized) default-frame-alist)
;;;    A.2. <f11> `(toggle-frame-fullscreen)' goes native fullscreen
;;;    A.3. M-<f10> `(toggle-frame-maximized) expands the frame to maximum'
;;; B. Denote Faces Title
;;;    B.1. Customize the face denote-faces-title to be "light green"
;;; C. Org.el function (org-store-log-note) should be changed:
;;;    (insert-and-inherit "\n" (org-list-bullet-string "-") (pop lines))
;;;                         ^^
;;;    This will insert a space before a new note.  It may insert a space
;;;    in every note, but that may not be problem.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set diary and bookmark files
;; Place diary into notes/ccvlp2 so it can saved in a secret repo
(setq diary-file "~/.local/share/notes/ccvlp/diary")
(diary)
(setq bookmark-default-file "~/.local/share/emacs/site-lisp/bookmarks.el")

;; Add the Super and Hyper modifer keys to Mac
(setq mac-right-option-modifier 'super)
(setq ns-function-modifier 'hyper)

;;; Helpful
;;  Global key bindings
;; (require 'helpful)
;; (keymap-global-set "C-h f"   #'helpful-callable) ; default #'describe-function
;; (keymap-global-set "C-h v"   #'helpful-variable) ; default #'describe-variable
;; (keymap-global-set "C-h k"   #'helpful-key)      ; default #'describe-key
;; (keymap-global-set "C-h x"   #'helpful-command)  ; default #'describe-command
;; (keymap-global-set "C-c C-d" #'helpful-at-point) ; Org #'org-deadline
;; (keymap-global-set "C-h F"   #'helpful-function) ; default #'Info-goto-emacs-command-node


(add-hook 'before-save-hook 'time-stamp t)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;;; See the issue of MuPDF having trouble rendering SVG
;;; Denote 20240128T082505
(add-hook 'doc-view-mode-hook (lambda ()
                                (setq doc-view-mupdf-use-svg nil)))


;; INFOPATH: make sure envvars.zsh points to /usr/local and /opt/local
(add-to-list 'Info-directory-list "~/.local/share/info/")
(add-to-list 'Info-directory-list "~/.local/src/emacs/denote")
(add-to-list 'Info-directory-list "~/.local/src/common-lisp/share/info")


;;; Local Utilities
;; Symlink local code into ~/.local/share/share/emacs/site-lisp
(require 'template-funcs)
(require 'extract)
(require 'helpers)
(require 'textproc)
(require 'noteproc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(setq inferior-lisp-program "sbcl")

(require 'sly)

(setf sly-lisp-implementations nil
      cl-implementations-path "~/.local/src/common-lisp/implementations"
      cl-implementations (list 'sbcl 'ccl 'abcl))

;;; TODO: add allowed options and keyword arguments
(with-eval-after-load 'sly
  (dolist (imp cl-implementations)
    (push (list imp (list cl-implementations-path))
          sly-lisp-implementations)))

(with-eval-after-load 'sly
  (keymap-set sly-prefix-map "M-h" 'sly-documentation-lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-id)
(require 'ox-texinfo)
(require 'ox-md)
;; (require 'ob-http)

(setq org-agenda-include-diary t
      org-attach-preferred-new-method 'dir
      org-attach-method 'lns
      org-attach-store-link-p 'attached
      org-clock-into-drawer "WORKTIME"
      org-log-note-clock-out t
      org-log-states-order-reversed nil
      org-startup-folded t
      org-time-stamp-rounding-minutes '(6 6)
      org-clock-rounding-minutes 6
      org-clock-persist 'history
      org-use-speed-commands t)

(org-clock-persistence-insinuate)


(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "C-c C-."
                        'org-time-stamp-inactive)))

;;; TASKS:      TODO(t@) NEXT(n!) HOLD(h@) | DONE(d!) CLOSED(c@) CANCELLED(C!)
;;; DOCUMENTS:  DRAFT-DOC(D@) DRAFT-LTR(L@) SIGN(s@) PRESENT(p@) | DRAFTED(F!) SIGNED(S!) PRESENTED(P@)
;;; HEARINGS:   HEARING(H@) | ATTENDED(A!)
;;; COURTESY:   COURTESY(C@) | PROVIDED(P!)
;;; DISCOVERY:  REQUEST(r@) WAIT(w@) | RECEIVED(v!) NOT-RCVD(N!)
;;; ASSIGN:     DELEGATED(g@) | ACCOMPLISHED(a!)

(setq org-todo-keywords
      '((sequence "TODO(t@)" "NEXT(n!)" "HOLD(h@)" "|" "DONE(d!)" "CLOSED(c@)" "CANCELLED(C!)")
        (sequence "DRAFT-DOC(D@)" "DRAFT-LTR(L@)" "SIGN(s@)" "PRESENT(p@)" "|" "DRAFTED(F!)" "SIGNED(S!)" "PRESENTED(P@)")
        (sequence "HEARING(H@)" "|" "ATTENDED(A!)")
        (sequence "COURTESY(C@)" "|" "PROVIDED(P!)")
        (sequence "REQUEST(r@)" "WAIT(w@)" "|" "RECEIVED(v!)" "NOT-RCVD(N!)")
        (sequence "DELEGATED(g@)" "|" "ACCOMPLISHED(a!)")))


;; (setq org-todo-keywords
;;       '((sequence "TODO(t@)" "WAIT(w@)" "HOLD(h@)" "|" "DONE(d!)" "RECEIVED(!)" "CANCELLED(c!)" )
;;         (sequence "DRAFT(D@)" "DELEGATE(@)" "|" "DRAFTED(!)" "DELEGATED(!)")
;;         (sequence "LETTER(l@)" "|" "WROTE(!)")
;;         (sequence "REQUEST(r)" "|" "REQUESTED(R!)")
;;         (sequence  "HEARING(H!)" "|" "ATTENDED(A@")
;;         (sequence "TASK(T!)" "NEXT(N@)" "|" "CLOSED(C@)")))


(setq org-agenda-files
      '("~/.local/share/notes/"
        "~/.local/share/notes/ccvlp/"
        "~/.local/share/notes/ccvlp/cases/"
        "~/.local/share/notes/ccvlp/law/"
        "~/.local/share/notes/legal/"
        "~/.local/share/notes/personal/"))

(setq org-default-notes-file "~/.local/share/notes/captured.org")

(setq org-publish-project-alist
      `(("law"
         :base-directory ,(file-name-concat (denote-directory) "law" "source")
         :publishing-directory ,(file-name-concat (denote-directory) "law" "publish" "law" "docs")
         :publishing-function org-md-publish-to-md
         :section-numbers nil)))

;; (setq org-static-blog-page-header nil)
;; (setq org-static-blog-page-preamble nil)
;; (setq org-static-blog-page-postamble nil)
;; (setq org-static-blog-index-front-matter nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mu/mu4e - Mail User Agent
;;; mu init -m ~/.local/share/mail --my-address me@gmail.com --my-address me@mac.com ...
;;; mu index
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html

;; (setq mail-user-agent 'mu4e-user-agent)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)

;; ;;; mu4e - config
;; (setq mu4e-get-mail-command "mbsync gmail" ; U to update from the mainview
;;       mu4e-maildir (expand-file-name "~/.local/share/mail")
;;       mu4e-attachment-dir (expand-file-name "~/Downloads")
;;       mu4e-compose-format-flowed t
;;       mu4e-html2text-command "w3m -T text/html" ; there are many options
;;       mu4e-update-interval 600
;;       mu4e-index-update-in-background t
;;       mu4e-headers-auto-update t
;;       mu4e-change-filenames-when-moving t
;;       mu4e-context-policy 'pick-first)

;; (setq mu4e-contexts
;;       (list
;;        (make-mu4e-context
;;         :name "CCVLP"
;;         :match-func
;;         (lambda (msg)
;;           (when msg (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
;;         :vars
;;         '((user-mail-address  . "lincoln@ccvlp.org")
;;           (user-full-name     . "W. Lincoln Harvey")
;;           (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
;;           (mu4e-sent-folder   . "/gmail/[Gmail]/Sent Mail")
;;           (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
;;           (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")
;;           (mu4e-sent-messages-behavior . delete)
;;                                         ; add a signature
;;                                         ; (mu4e-compose-signature . "...")
;;           (smtpmail-smtp-server . "smtp.gmail.com")))
;;        ;; (make-mu4e-context
;;        ;;  :name "LOLH"
;;        ;;  :match-func
;;        ;;  (lambda (msg)
;;        ;;    (when msg (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
;;        ;;  :vars
;;        ;;  '((user-mail-address  . "lincolnlaw@mac.com")
;;        ;;    (user-full-name     . "W. Lincoln Harvey")
;;        ;;    (mu4e-refile-folder . "/icloud/Archive")
;;        ;;    (mu4e-sent-folder   . "/icloud/Sent Messages")
;;        ;;    (mu4e-drafts-folder . "/icloud/Drafts")
;;        ;;    (mu4e-trash-folder  . "/icloud/Deleted Messages")
;;        ;;    (mu4e-sent-messages-behavior . sent)
;;        ;;                                  ; add a signature
;;        ;;                                  ; (mu4e-compose-signature . "...")
;;        ;;    (smtpmail-smtp-server . "smtp.mail.me.com")))
;;        ))

;; ;;; mu4e - shortcuts to the folders; show up in the mode line
;; (setq mu4e-maildir-shortcuts
;;       '((:maildir "/gmail/Inbox"                :key ?i)
;;         (:maildir "/gmail/[Gmail]/All Mail"     :key ?a)
;;         (:maildir "/gmail/[Gmail]/Sent Mail"    :key ?s)
;;         (:maildir "/gmail/[Gmail]/Trash"        :key ?t)
;;         (:maildir "/gmail/[Gmail]/Drafts"       :key ?d)))

;; ;;; smtpmail - config
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 nil nil))
;;       ;;      smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtp-debut-info t
;;       message-kill-buffer-on-exit t)

;; (define-key global-map (kbd "C-c n m") #'mu4e-org-store-and-capture)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Denote

;; Do not use the Packages version; place ~denote~ into ~/.local/src/emacs
;; update this using `git-pull' regularly

(require 'denote)

(setq denote-directory (expand-file-name "~/.local/share/notes")
      denote-prompts '(title keywords signature subdirectory template)
      denote-date-prompt-use-org-read-date t
      ;; see 'denote-dired-mode-in-directories'
      denote-dired-directories-include-subdirectories t
      denote-save-buffer-after-creation t)

;; Denote Dired Mode setup
(setq denote-dired-directories ; use denote-dired-mode in these directories
      (list
       denote-directory
       (expand-file-name "~/.local/share/notes/ccvlp")
       (expand-file-name "~/.local/share/notes/ccvlp/cases")
       (expand-file-name "~/.local/share/notes/ccvlp/cases/closed")
       (expand-file-name "~/.local/share/notes/ccvlp/clients")
       (expand-file-name "~/.local/share/notes/ccvlp/attorneys")
       (expand-file-name "~/.local/share/notes/ccvlp/law")
       (expand-file-name "~/.local/share/notes/legal")
       (expand-file-name "~/.local/share/notes/personal")))

(setq denote-silo-extras-directories
      '("~/.local/share/notes/ccvlp"
        "~/.local/share/notes/ccvlp/cases"
        "~/.local/share/notes/ccvlp/cases/closed"
        "~/.local/share/notes/ccvlp/clients"
        "~/.local/share/notes/ccvlp/law"
        "~/.local/share/notes/legal"
        "~/.local/share/notes/personal"))

(setq denote-journal-extras-directory
      (file-name-concat (denote-directory) "personal" "journal"))

(setq denote-templates
      `((blank . ,(blank))
        (client . ,(newclient))
        (case . ,(newcase))
        (checklist . ,(checklist))
        (recipe . ,(recipe))
        (journal . journal)))

(add-hook 'dired-mode-hook
          (lambda ()
            (progn
              (when (diredfl-mode)
                (diredfl-mode -1)) ; dired-mode does not work with diredfl
              (denote-dired-mode-in-directories) ; fontify the directory file names
              (custom-set-faces '(denote-faces-title ((t (:foreground "green3")))))
              (custom-set-faces '(denote-faces-date ((t (:foreground "yellow"))))))))

(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "C-c n J") #'denote-journal-extras-new-entry)
  (define-key map (kbd "C-c n K") #'denote-journal-extras-new-or-existing-entry)
  (define-key map (kbd "C-c n M") #'denote-journal-extras-link-or-create-entry)
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
  ;; Denote Extras
  (define-key map (kbd "C-c n C") #'denote-silo-extras-create-note) ; Create
  (define-key map (kbd "C-c n O") #'denote-silo-extras-open-or-create) ; Open-or-Create
  (define-key map (kbd "C-c n S") #'denote-silo-extras-select-silo-then-command)) ; Select-then-Command

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
  (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

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


;;; Sample org-capture
(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("N" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORG-STATIC-BLOG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-static-blog-publish-title "HJP and RTC Legal Resources"
      org-static-blog-publish-url "https://hjp.org"
      org-static-blog-publish-directory (file-name-concat (denote-directory) "law" "osb" "html")
      org-static-blog-posts-directory (file-name-concat (denote-directory) "law" "osb" "html" "posts")
      org-static-blog-drafts-directory (file-name-concat (denote-directory) "law" "osb" "html" "drafts")
      org-static-blog-enable-tags t
      org-static-blog-use-preview t
      org-export-with-toc t
      org-export-with-section-numbers nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEDGER
;; (require 'ledger-mode)
(add-hook 'ledger-mode-hook
          (lambda ()
            (setq-local ledger-default-date-format ledger-iso-date-format)
            (setq-local tab-always-indent 'complete)
            (setq-local completion-cycle-threshold t)
            (setq-local ledger-complete-in-steps t)
            (setq-local ledger-copy-transaction-insert-blank-line-after t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom functions

(defun init-emacs ()
  "Open the init.el file in a new frame for editing."
  (interactive)
  (find-file "~/.config/emacs/lisp/init-local.el"))

(keymap-global-set "C-c _" #'lolh/underscore)

(defun lolh/underscore ()
  "Insert a line of underscores (actually dashes) beneath a line of text.

  Point must be in the line beneath which the underscores will be added."

  (interactive)

  (let ((line-len (- (pos-eol) (pos-bol))))
    (newline 1)
    (insert-char ?- line-len)
    (newline 1)))


(provide 'init-local)
;;; init-local.el ends here
