;;; ~/.local/share/emacs/init.el --- Emacs init file -*- Mode: emacs-lisp; -*-
;;; Time-stamp: <2023-11-22 21:03:25 minilolh>

;;; ====================================================================

;;; NOTE: The file in ~/.config/emacs/init.el should be a sym-link
;;;       of the original file in ~/.local/share/emacs/init.el.
;;;       When the function `init-emacs' is run, it opens the sym-link
;;;       and edits the original.

;;; ====================================================================


;;; EMACS
;;; -----
;;  Gnu Emacs               => https://www.gnu.org/software/emacs/
;;  Emacs Git Repositority  => https://savannah.gnu.org/git/?group=emacs
;;  Emacs source repository => https://git.savannah.gnu.org/cgit/emacs.git
;;  GNU ELPA       => https://git.savannah.gnu.org/cgit/emacs/elpa.git
;;  NonGNU ELPA    => https://git.savannah.gnu.org/cgit/emacs/nongnu.git
;;  Emacs Org mode => https://git.savannah.gnu.org/cgit/emacs/org-mode.git
;;  Building Emacs => https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Emacs.htlm

;;; EMACS PACKAGES
;;; --------------
;;
;;  A `package'  is a collection of  one or more files,  formatted and
;;  bundled in  such a  way that users  can easily  download, install,
;;  uninstall, and upgrade it.
;;
;;  A `package' is either a simple  package or a multi-file package. A
;;  simple package  is stored in a  package archive as a  single Emacs
;;  Lisp file,  while a  multi-file package  is stored  as a  tar file
;;  (containing multiple Lisp files,  and possibly non-Lisp files such
;;  as a  manual).  The only difference  between them is how  they are
;;  created.
;;
;;  `Package-Attributes'
;;  - Name
;;  - Version
;;  - Brief Description
;;  - Long Description
;;  - Dependencies
;;
;;  RESOURCES FROM THE EMACS MANUAL
;;  ===============================
;;
;;  49. Emacs Lisp Packages--About
;;    => https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
;;
;;  49.3 Package Installation
;;    => https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
;;
;;  RESOURCES FROM THE ELISP MANUAL
;;  ===============================
;;
;;  42.1.1 Summary: Sequence of Actions at Startup
;;    => https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;;
;;  43. Preparing Lisp code for distribution
;;    => https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html
;;
;;  43.1 Elisp Package--Basics
;;    => https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging-Basics.html
;;
;;  FROM THE EMACSWIKI
;;  ==================
;;
;;  Listing Packages for installation
;;  - command `list-packages'
;;    - i :: mark for installation
;;    - u :: unmark from installation
;;    - x :: perform installation
;;
;;  `paradox' package => `paradox-list-packages' to sort packages by popularity.
;;
;;  MAKING PACKAGES
;;  ===============
;;  EmacsWiki => https://www.emacswiki.org/emacs/MakingPackages
;;
;;  MAKING PACKAGES--DEMO-MULTIFILE
;;  ===============================
;;  => https://github.com/nicferrier/elmarmalade/tree/master/demo-multifile
;;
;;;  CASK => https://github.com/cask/cask
;;;  ====
;;   Project management tool for Emacs
;;
;;
;;  SUMMARY OF PACKAGE INSTALLATION AND LOADING
;;  ===========================================;
;;
;;  Installing Packages using ELPA
;;    => https://www.emacswiki.org/emacs/InstallingPackages#installing-packages
;;
;;    INSTALLING A PACKAGE
;;    ====================
;;
;;    `package-install' <PKG> ::
;;    ==========================
;;    Install the  package `PKG'. `PKG'  can be a ‘package-desc’  or a
;;    symbol naming  one of  the available packages  in an  archive in
;;    ‘package-archives’.   See   also  `package-delete'.    Mark  the
;;    installed    package   as    selected    by    adding   it    to
;;    ‘package-selected-packages’.
;;
;;    `package-install-file' <FILE> ::
;;    Packages are installed using `package-install-file' <FILE>.
;;    Install a package from FILE. The  file can either be a tar file,
;;    an Emacs Lisp file, or a directory.
;;
;;    `package-install-selected-packages' ::
;;    Ensure packages in ‘package-selected-packages’ are installed. If
;;    some packages are not installed, propose to install them.
;;
;;    INSTALLATION PROCESS
;;    ====================
;;
;;    Installing   a   package  (`package-install-file')   creates   a
;;    subdirectory of `package-user-dir'  named `name-version' This is
;;    the  package’s "content  directory".  Emacs  puts the  package’s
;;    contents in  this directory (the  single Lisp file for  a simple
;;    package, or the files extracted from a multi-file package
;;
;;    Emacs then searches every Lisp file in the content directory for
;;    `autoload' magic comments.  These autoload definitions are saved
;;    to a file named `name-autoloads.el' in the "content directory".
;;
;;    Emacs then byte-compiles every Lisp  file in the package.
;;
;;    LOADING A PACKAGE
;;    =================
;;
;;    After installation, the installed  package is loaded: Emacs adds
;;    the package’s "content directory"  to `load-path', and evaluates
;;    the autoload definitions in `name-autoloads.el'.
;;
;;    `package-initialize': This function  initializes Emacs’ internal
;;    record  of   which  packages  are  installed,   and  then  calls
;;    `package-activate-all'.
;;
;;    Whenever Emacs  starts up,  it automatically calls  the function
;;    `package-activate-all' to  make installed packages  available to
;;    the current session.  This is done after loading  the early init
;;    file, but before loading the regular init file.
;;
;;    The user option `package-load-list'  specifies which packages to
;;    make  available; by  default,  all installed  packages are  made
;;    available.
;;
;;
;; CONVENTIONAL HEADERS FOR EMACS LIBRARIES
;; ----------------------------------------
;;
;; D.8 Conventional Headers
;;    => https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.htlm
;;
;;    filename --- description  -*- lexical-binding: t; -*-
;;
;;    Copyright (C) 2010-2021 Your Name
;;    *Author: Your Name <yourname@example.com>
;;    Maintainer: The Name <thename@example.com>
;;    Created: DATE
;;    Version/Package-Version:
;;    *Keywords:
;;    URL/Homepage:
;;    Package-Requires:
;;
;;    Commentary:
;;
;;    Change Log:
;;
;;    Code:
;;
;;    filename ends here
;;
;;
;;; PACKAGE ARCIVES
;;; ---------------
;;
;;; ELPA => https://elpa.gnu.org
;;; ----
;;; ELPA Package List => https://elpa.gnu.org/packages/
;;  ELPA Package Repo => https://git.savannah.gnu.org/cgit/emacs/elpa.git
;;  ELPA Wiki         => https://www.emacswiki.org/emacs/ELPA
;;  ELPA is the Emacs Lisp Package Archive and is the default repository used by `package.el'
;;  package.el is the package manager library for ELPA.
;;  How Packages Work = >https://www.emacswiki.org/emacs/ELPA
;;  http://savannah.gnu.org  is  a  hosting site  for  "official"  GNU
;;  software (i.e. sponsored by the Free Software Foundation).

;;; NonGNU ELPA => https://elpa.nongnu.org
;;; -----------
;;  NonGNU ELPA Package List => https://elpa.nongnu.org/nongnu/
;;  NonGNU ELPA Package Repo => https://git.savannah.gnu.org/cgit/emacs/nongnu.git
;;  http://savannah.nongnu.org  is  a  hosting  site  for  "community"
;;  projects that are not sponsored by the FSF.
;;  SEE => https://emacsredux.com/blog/2021/08/02/nongnu-elpa-package-repository/

;;; MELPA => https://melpa.org/
;;; -----
;;  MELPA GitHub => https://github.com/melpa/melpa
;;  MELPA Wiki   => https://www.emacswiki.org/emacs/MELAP
;;  MELPA is  an ELPA-compatible  package repository that  contains an
;;  enormous number of useful Emacs packages.
;;  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;
;;
;;; `display-graphic-p' returns t if DISPLAY is a graphic display and
;;; nil for text terminals

;;; Emacs initialization
;;; --------------------
;;; `user-init-file' contains the absolute file name of the init file:
;;;     e.g., ~/.config/emacs/init.el (which should be a sym-link)
;;; `user-emacs-directory' contains the Emacs default directory and defaults to
;;;     ~/.config/emacs/ (so long as ~/.emacs.d/ and ~/.emacs do not exist).
;;; Emacs loads the init files in the following order:
;;;     `early-init.el', `site-start.el', `init.el', and finally `default.el'.
;;; Emacs --debug-init =>
;;;     Enables the Emacs Lisp debugger for errors in the init file
;;; Emacs --iconic =>
;;;     Starts Emacs in an iconified state
;;; Emacs -Q =>
;;;    --no-init-file --no-site-file --no-site-lisp --no-x-resources --no-splash
;;; Emacs -q | --no-init-file =>
;;;     avoids all init files except `site-start.el'
;;; Emacs --no-site-file =>
;;;     avoid's loading `site-start.el'
;;; Emacs --init-directory =>
;;;     to use a different init file
;;; set `inhibit-default-init' to t to not load `default.el'

;;; Early Init File => ~/.config/emacs/early-init.el
;;; ---------------
;;; This file is loaded before the package system and GUI is initialized.
;;; Customize variables that affect the package initialization process, such as
;;; - `package-enable-at-startup'
;;; - `package-load-list'
;;; - `package-user-dir'

;;; Scripting with Emacs
;;; --------------------
;;; #!/path/to/emacs --script =>
;;;     invokes Emacs with `---script' and supplies the name
;;; of the script as FILE.
;;; #!/path/to/emacs -x =>
;;;     like --script but suppresses loading the init files (like --quick)

;;; Emacs Load Path
;;; ---------------
;;; Load Path => `load-path'
;;; (add-to-list 'load-path "/path/to/lisp/libraries")
;;; (load "foo") => `load' searches the directories in `load-path' for "for"
;;; (autoload 'myfunction "mypackage" "Doc string" t)
;;;     => Find the definition for `myfunction'
;;;     by loading the Lisp library named `mypackage'
;;; => Make the doc string available for help commands immediately.
;;; => t makes the command interactive.

;;; $PATH => `exec-path'

;;; Rebinding of keys:
;;; ------------------
;;; (keymap-global-set "C-x l" 'make-symbolic-link)
;;; (keymap-set global-map "C-x l" 'make-symbolic-link)
;;; (keymap-set lisp-mode-map "C-x l" 'make-symbolic-link)
;;; (kaymap-global-unset "C-x C-v")

;;; Server
;;; ------
;;; `(require 'server)' must be invoked first.
;;; `server-process' returns the server object if one is running.
;;; `(server-running-p)' returns t if a server is running
;;; `server-name' is the name of the server currently running
;;;	If this name is an absolute path, place the socket there.
;;; `server-socket-dir'
;;;	is the directory containing the local server socket files.
;;;	It's value is like:
;;;		"/var/folders/m_/y9gh17jd5tgd12f1qfh5qpf80000gn/T/emacs501"
;;; `(server-force-delete)' deletes the server .
;;; `(kill-emacs)' =>
;;;	kills Emacs and deletes the server socket file unconditionally.


;;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


(require 'server)

;;; Start a GUI server unless one is already running.
(let ((servers (directory-files server-socket-dir nil "server")))
  ;; `servers' holds a list of the server socket names,
  ;; e.g. (guiserver termserver)
  (message "Emacs USER login name is %s" (getenv "USER"))
  (message "Emacs HOME directory is %s" (getenv "HOME"))
  (message "The System name is %s" system-name)
  (message "Server-socket-dir is %s" server-socket-dir)
  (message "Running servers are: %s" (directory-files server-socket-dir nil "server"))
  (when (display-graphic-p)
    (unless (member "guiserver" servers)
      ;; `guiserver' is not found, so create and start it
      (setq server-name "guiserver")
      (server-start)
      ;; Use C-z to toggle the Emacs frame
      (suspend-frame))))


;;; Graphical and Display defaults
;;  Can also use `initial-frame-alist'
(add-to-list 'default-frame-alist '(top . 100))
(add-to-list 'default-frame-alist '(left . 300))
(add-to-list 'default-frame-alist '(width  . 200))
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(font . "Monospace-14"))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "*Scratch* Buffer

")

(global-display-line-numbers-mode)
(column-number-mode)
(setq-default indent-tabs-mode nil)

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
;(add-hook 'text-mode-hook 'whitespace-mode)
;(add-hook 'prog-mode-hook 'whitespace-mode)

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(add-hook 'before-save-hook 'time-stamp)


;;; Slime & Quicklisp
;;  Quicklisp must first be installed; place it into
;;    ~/.local/share/common-lisp/quicklisp
;;  Also install `clhs' : (ql:quickload 'clhs)
;;  Then run (clhs:print-emacs-setup-form) and follow the instructions.
;;  Evaluate C-c C-d h make-instance RET to test if the change was successful.
;;  Evaluate C-c C-d ~ to look up a format directive, e.g., `~A'
(load (expand-file-name "~/.local/share/common-lisp/quicklisp/slime-helper.el"))
(load (expand-file-name "~/.local/share/common-lisp/quicklisp/clhs-use-local.el") t)
;;(setq inferior-lisp-program "sbcl")
(setq slime-lisp-implementations
      '((ccl ("ccl"))
	(sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
	(clisp ("ccl"))
	(abcl ("abcl"))))

;;; Roswell and Slime
(load (expand-file-name "~/.local/share/common-lisp/roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run")


;;; Org Mode
;; Enable global keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c C-x C-o") #'org-clock-out)
(global-set-key (kbd "C-c C-x C-x") #'org-clock-in-last)

(require 'org-tempo)

;; Disable confirmation of evaluation requests
(setq org-confirm-babel-evaluate nil) ; disable confirmation requests
(setq org-link-shell-confirm-link-function nil) ; disable confirmation requests
(setq org-link-elisp-confirm-function nil) ; disable confirmation requests


;;; Org Babel
;; Activate additional languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (lisp . t)))


;;; Paredit Mode
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; Rust-mode
;;; https://github.com/rust-lang/rust-mode
;;; C-c C-c C-u rust-compile
;;; C-c C-c C-k rust-check
;;; C-c C-c C-t rust-test
;;; C-c C-c C-r rust-run
;;; C-c C-c C-l rust-run-clippy (a linter)
;;; C-d C-d rust-dbg-wrap-or-unwrap
;;; rust-toggle-mutability
;;; See rustic => https://github.com/brotzeit/rustic
; (require 'rust-mode)


;;; hledger-mode
;;; https://github.com/narendraj9/hledger-mode
(require 'ledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))
;(setq hledger-jfile "~/.local/work/workfin/hledger/workfin.journal")
;; Personal Accounting
;(global-set-key (kbd "C-c e") 'hledger-jentry)
;(global-set-key (kbd "C-c j") 'hledger-run-command)

;; ;;; Auto-completion for account names
;; ;; For company-mode users,
;; (add-to-list 'company-backends 'hledger-company)

;; ;; For auto-complete users,
;; (add-to-list 'ac-modes 'hledger-mode)
;; (add-hook 'hledger-mode-hook
;;     (lambda ()
;;         (setq-local ac-sources '(hledger-ac-source))))


;;; flymake-hledger  This Emacs  package  is a  Flymake’s backend  for
;;; hledger:   it  reports   errors   in   your  accounting   journal.

;;; https://github.com/DamienCassou/flymake-hledger

;;; To   enable    Flymake   in   the   current    buffer,   run

;;; M-x flymake-hledger-enable.

;;; Feel  free  to add  this  command  to  the  special eval  file  or
;;; directory-local variables.


;;; denote
(require 'denote)
(setq denote-directory (expand-file-name "~/.local/share/denote/notes/"))


;;; Mail

;;; isync/mbsync IMAP synchronizer
;; .mbsyncrc

;;; mu/mu4e
;; mu init -m ~/.local/share/mail --my-address me@gmail.com --my-address me@mac.com ...
;; mu index

(setq ;user-full-name     "W. Lincoln Harvey"
      ;user-mail-address  "lincoln@ccvlp.org"
      mail-user-agent 'mu4e-user-agent)

;; (setq message-signature ; t => message-signature-file
;;       (concat
;;        "\nW. Lincoln Harvey\n"
;;        "*Eviction Defense Staff Attorney*\n"
;;        "*Clark County Volunteer Lawyers Program*\n"
;;        "1104 Main Street, Suite 212\n"
;;        "Vancouver, WA 98660\n"
;;        "Direct: 360-356-7872 x 203\n\n"

;;        "www.ccvlp.org\n"
;;        "https://www.facebook.com/VolunteerLawyers\n"
;;        "https://twitter.com/CCVLawyers\n"
;;        "https://www.instagram.com/volunteerlawyers\n\n"

;;        "*CONFIDENTIALITY NOTICE:*
;; This transmission and any documents that accompany it may contain information belonging
;; to the sender that is protected by attorney-client privilege. This information is confidential.\n\n"

;;        "*RESTRICTED USE:* 
;; You may not use the information in this transmission in any way if you are not the intended
;; recipient. Do not read any part of this transmission if you are not the person it was directed
;; to. Call us immediately to arrange for a return of the documents if you receive this transmission
;; in error.\n\n"
;;        ))

;;; mu4e - Mail User Agent
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;;; mu4e - config
(setq mu4e-get-mail-command "mbsync -a" ; U to update from the mainview
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
          (mu4e-refile-filder . "/gmail/[Gmail]/All Mail")
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
          (mu4e-refile-filder . "/icloud/Archive")
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
      (:maildir "/icloud/Inbox"               :key ?I)
      (:maildir "/gmail/[Gmail]/All Mail"     :key ?a)
      (:maildir "/gmail/[Gmail]/Sent Mail"    :key ?s)
      (:maildir "/icloud/Sent Messages"       :key ?S)
      (:maildir "/gmail/[Gmail]/Trash"        :key ?t)
      (:maildir "/icloud/Deleted Messages"    :key ?T)
      (:maildir "/gmail/[Gmail]/Drafts"       :key ?d)
      (:maildir "/icloud/Drafts"              :key ?D)))

;;; mu4e - Bookmarks; show up in the menu as predefined queries
;; (add-to-list 'mu4e-bookmarks
;;              '(:name "Gmail Inbox"
;;                :query "maildir:/gmail/INBOX"
;;                :key ?i
;;                :favorite t))

;; (add-to-list 'mu4e-bookmarks
;;              '(:query "maildir:/icloud/Inbox" :name "iCloud Inbox" :key ?I :favorite t))


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("/opt/local/share/info"))
 '(calendar-date-style 'iso)
 '(custom-enabled-themes '(wombat))
 '(denote-directory "/Users/minilolh/.local/share/org/denote/")
 '(denote-prompts '(title keywords signature))
 '(gnus-save-killed-list nil)
 '(org-agenda-files
   '("~/.local/ccvlp/org/rtc.org" "~/.local/ccvlp/org/denote/"))
 '(org-attach-method 'cp)
 '(org-default-notes-file "~/.local/share/ccvlp/org/notes.org")
 '(org-log-into-drawer t)
 '(org-log-note-clock-out t)
 '(org-log-states-order-reversed nil)
 '(org-time-stamp-rounding-minutes '(6 6))
 '(package-selected-packages
   '(pdf-tools markdown-mode flymake-hledger paradox denote tmr magit sml-mode paredit))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; Local Variables:
;;; End:

;;; init.el end
