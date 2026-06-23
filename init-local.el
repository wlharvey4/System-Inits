;;; init-local.el --- Local Lisp support -*- lexical-binding: t -*-
;;; Time-stamp: <2026-06-16 15:13:43 lolh-mbp-16>

;;; Commentary:
;;; init-local.el

;;; This code covers the following local configurations:
;;;
;;; 0. purcell/emacs.d => ~/.local/share/emacs/purcell-emacs.d/
;;;    0.1. ~/.config/emacs is a symlink to purcell-emacs.d/
;;;    0.2 ~/.local/share/emacs/purcell-emacs.d/list
;;;
;;; 1. mu/mu4e => ~/.local/share/mu installed into /usr/local/
;;;    1.1. mu is installed in /usr/local/bin
;;;    1.2. mu4e is installed into /usr/local/share/emacs/site-lisp
;;;    1.3./usr/local/share/emacs/site-lisp/mu4e must be added to load-path
;;;    1.4. mu4e.info  is installed into /usr/local/share/info
;;;    1.5. /usr/local/share/info must be added to INFODIR
;;;    1.6. Configure mu4e
;;;
;;; 2. Denote => ~/.local/src/emacs/denote/
;;;    2.1. README.org needs to be compiled into denote.info, and installed into dir
;;;    2.2. Add key bindings
;;;    2.3. Set up default denote directory => ~/.local/share/notes
;;;    2.4. Set up silos
;;;          i. ccvlp
;;;         ii. law
;;;        iii. personal
;;;         iv. languages
;;;         iv.a french
;;;         iv.b german
;;;
;;; 3. Org
;;;    3.1. require ox-texinfo to be able to export to info files
;;;    3.2. org-attach-method needs to be set of lns
;;;    3.3. set org-indent-mode to get rid of multiple stars in headings
;;;    3.4. Add key C-c C-. for an inactive time stamp
;;;    3.5. Add todo keywords
;;;    3.6. Add org agenda files
;;;    3.7 initlocal/org-babel-tangle-config=>automatically tangle literate files with special keyword
;;;
;;; 4. Diary
;;;    4.1. Set diary file to ~/.local/share/emacs/diary
;;;
;;; 5. Emacs
;;;    5.1. time-stamp
;;;    5.2. visual-line-mode
;;;    5.3. dired-hide-details-mode
;;;    5.4. bookmark-default-file => ~/.local/share/emacs/site-list/bookmarks.el
;;;           symlink ~/.local/src/System-Inits/bookmarks.el to
;;;           ~/.local/share/emacs/site-list/bookmarks.el
;;;    5.5. Accent-Map custom keybindings
;;;
;;; 6. Local Emacs Code should be Symlinked into a Site Lisp directory
;;;    6.1. ~/.local/src/emacs/utils/template-funcs -> ~/.local/share/emacs/site-lisp/template-funcs
;;;    6.2, ~/.local/src/emacs/utils/extract -> ~/.local/share/emacs/site-lisp/extract
;;;
;;; 7. Common Lisp
;;;    7.1 Prefix is ~/.local/src/common-lisp
;;;    7.2 bin is ~/.local/src/common-lisp/bin
;;;    7.3 implementations at ~/.local/source/common-lisp/implementations
;;;
;;; 8. Language Accents in French, German, and Spanish | 2026-06-04T12:00
;;;    8.1 (global-set-key (kbd "C-c f f") (lambda () (interactive) (activate-input-method "french-prefix")))
;;;    8.2 (global-set-key (kbd "C-c f s") (lambda () (interactive) (activate-input-method "spanish-prefix")))
;;;    8.3 (global-set-key (kbd "C-c f g") (lambda () (interactive) (activate-input-method "german-prefix")))
;;;    8.4 Type C-c l f to review or write French.
;;;    8.5 Hit C-\ to instantly toggle back to standard English for regular typing or coding.
;;;    8.6 Type C-c l s when you want to switch over to Spanish, and your fingers will already know exactly what to do.
;;;
;;; 9. Dictionaries and auto-writeroom-mode for Denote file editing
;;;    2026-06-16
;;; Fantastic! That is the ultimate feeling in Emacs configuration—when the startup scripts balance perfectly, the bindings drop into place globally, and the system just *works* from the very first frame.

;;; You now have a beautifully streamlined, highly custom linguistic and text-processing environment. Let's do a quick victory-lap review of what you just built:

;;; The Matrix Core:** A completely offline translation engine mapping nearly 200,000 words across German, Swedish, and French (via that massive 79k Wiktionary upgrade).
;;; The Intelligence Layer:** A regex processor that completely strips raw web code out of the backend data and automatically trims down complex French elisions (`l'`, `d'`) to isolate the root lemma on the fly.
;;; The Sandbox Prefix:** A persistent global keyboard hub bound to **`C-x D`** that is universally alive the second Emacs launches.
;;; The Flow State Auto-Toggle:** A smart-path directory listener bound to **`C-x D w`** that auto-snaps your newly generated or opened Denote files directly into distraction-free focus mode when you start your sessions.

;;; Everything is isolated, clean, and entirely under your local terminal and script control. Enjoy the absolute fluidity of your new setup during your deep-focus writing and reading sessions! You earned this win.

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
;;;
;;; NOTES:
;;; 2026-03-12T0825 I set up Git system-wide to rebase PULLS with the following:
;;;   git config --global pull.rebase true
;;;   to test:
;;;   git config --global pull.rebase run as a command should return "true"
;;;   git config --list --show-origin | grep pull.rebase to see the file path with that config

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
(add-to-list 'Info-directory-list "~/.local/src/emacs/denote-org")
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

;;; (require 'sly)

(setf sly-lisp-implementations nil
      cl-implementations-path "~/.local/src/common-lisp/implementations/bin"
      cl-implementations (list 'abcl 'sbcl 'ccl))

;;; TODO: add allowed options and keyword arguments
(with-eval-after-load 'sly
  (dolist (imp cl-implementations)
    (push (list imp (list (expand-file-name (prin1-to-string imp) cl-implementations-path)))
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
      org-use-speed-commands t
      org-refile-targets '((nil . (:level . 3)))) ; For the benefit of Refiling while running Denote

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
        "~/.local/share/notes/ccvlp/clients/"
        "~/.local/share/notes/ccvlp/hjp/"
        "~/.local/share/notes/law/"
        "~/.local/share/notes/personal/"))

(setq org-default-notes-file "~/.local/share/notes/captured.org")

(setq org-publish-project-alist
      `(("law"
         :base-directory ,(file-name-concat (denote-directory) "law" "source")
         :publishing-directory ,(file-name-concat (denote-directory) "law" "publish" "law" "docs")
         :publishing-function org-md-publish-to-md
         :section-numbers nil)))


;;; org-babel-tangle-config
;; 2026-03-13T23:00

;; This is a custom function that tangles your Org files automatically
;; every  time you  save  an  Org buffer.   To  automate  this, use  a
;; buffer-local hook:  after-save-hook.  This  ensures that  only your
;; specific configuration files tangle on  save, rather than every Org
;; file you happen to open.

;; How to use it:
;; Just add  #+auto_tangle: t anywhere  in your Org file  header. Now,
;; every  time you  press C-x  C-s, Emacs  will instantly  update your
;; tangled file at the path resolved by (file-truename "~").

;; Why this is safer for your workflow

;; Non-Blocking: If org-babel-tangle-collect-blocks returns a format
;; the code doesn't recognize (e.g., if it stops being an Alist), the
;; condition-case will catch the error. You’ll see a message in the
;; minibuffer, but you won't be stuck with a broken "save" process.

;; Debug-Ready: The (error-message-string err) will tell you exactly
;; what went wrong (e.g., "Wrong type argument: listp, some-value"),
;; which makes it much easier to fix if Org-mode updates again.

;; Dual-Machine Reliability: Since you're moving between MacBooks,
;; this safety net is vital. If one machine has an older version of
;; Org and the other has a newer one, this function won't break your
;; init.el on either.

;;The Smart Auto-Tangle Hook
(defun init-local/org-babel-tangle-config ()
  "Tangle and add aligned Denote-style metadata headers to the top of the file."
  (when (and (eq major-mode 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "#\\+auto_tangle: t" nil t)))
    (condition-case err
        (let* ((org-confirm-babel-evaluate nil)
               ;; Use 'car' to extract the string path from your Org version's list structure
               (all-targets (delete-dups (mapcar #'car (org-babel-tangle-collect-blocks)))))

          (org-babel-tangle t)

          (dolist (target all-targets)
            (when (stringp target)
              (let* ((dest (expand-file-name target))
                     (fname (file-name-nondirectory dest))
                     (fext (file-name-extension dest))
                     (mode-str (cond ((string= fext "el") "emacs-lisp")
                                     ((string= fext "sh") "shell-script")
                                     ((string= fext "zsh") "sh")
                                     (t "text")))
                     ;; Define headers in a list for easier alignment calculation
                     (headers `(("+FILENAME:"    . ,fname)
                                ("+SYSTEM-NAME:" . ,(system-name))
                                ("+USER-NAME:"   . ,(user-login-name))
                                ("+Time-stamp:"  . ,(format "<%s>" (format-time-string "%Y-%m-%d %H:%M:%S")))
                                ("+mode:"        . ,mode-str)))
                     ;; Calculate the length of the longest header key
                     (max-key-len (apply #'max (mapcar (lambda (h) (length (car h))) headers))))

                (when (file-exists-p dest)
                  (with-current-buffer (find-file-noselect dest)
                    (if (fboundp 'flymake-mode) (flymake-mode -1))
                    (if (fboundp 'flycheck-mode) (flycheck-mode -1))

                    (save-excursion
                      (goto-char (point-min))
                      ;; 1. Remove any old Denote-style header lines
                      (while (looking-at "^[;#]+\\+\\(FILENAME\\|SYSTEM-NAME\\|USER-NAME\\|Time-stamp\\|mode\\):.*\n")
                        (delete-region (point) (line-beginning-position 2)))

                      ;; 2. Insert new aligned headers
                      (let ((c (if (string= fext "el") ";;" "#")))
                        (dolist (h headers)
                          (let* ((key (car h))
                                 (val (cdr h))
                                 ;; Padding: (Max Length - Current Key Length) + 1 extra space
                                 (padding (make-string (+ 1 (- max-key-len (length key))) ?\s)))
                            (insert (format "%s%s%s%s\n" c key padding val))))
                        (insert (format "%s\n" c)))) ; Add a trailing empty comment line for spacing
                    (save-buffer)
                    (kill-buffer))))))
          (message "Tangle complete: Aligned Denote-style headers applied."))
      (error (message "Auto-tangle failed: %s" (error-message-string err))))))

(add-hook 'after-save-hook #'init-local/org-babel-tangle-config)

;; 4. Bonus: Keybinding to jump to the output folder
(global-set-key (kbd "C-c o c")
                (lambda () (interactive)
                  (dired (expand-file-name ".emacs.d/" (file-truename "~")))))


;; The "Jump to System-Inits directory" Function

;; Add this to your init-local.el. It uses (file-truename "~") to
;; ensure it always finds the right path, even if you move between
;; your two MacBooks.

;; C-c o d
(defun init-local/open-tangle-directory ()
  "Open the directory where my tangled config files live."
  (interactive)
  (let ((target-dir (file-truename "~/.local/src/System-Inits/")))
    (if (file-directory-p target-dir)
        (dired target-dir)
      (message "Directory %s does not exist!" target-dir))))

;; Bind it to a key for quick access
(global-set-key (kbd "C-c o d") #'init-local/open-tangle-directory)


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
(require 'denote-org)

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
       (expand-file-name "~/.local/share/notes/personal")
       (expand-file-name "~/.local/share/notes/languages")
       (expand-file-name "~/.local/share/notes/languages/french")
       (expand-file-name "~/.local/share/notes/languages/german")))

(setq denote-silo-extras-directories
      '("~/.local/share/notes/ccvlp"
        "~/.local/share/notes/ccvlp/cases"
        "~/.local/share/notes/ccvlp/cases/closed"
        "~/.local/share/notes/ccvlp/clients"
        "~/.local/share/notes/ccvlp/law"
        "~/.local/share/notes/legal"
        "~/.local/share/notes/personal"
        "~/.local/share/notes/languages"
        "~/.local/share/notes/languages/french"
        "~/.local/share/notes/languages/german"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prefix Mapping for Accented Characters (instead of the accent-map
;;; below
;;; 2026-06-04T10:30

;;; french-prefix | spanish-prefix | german-prefix
;;; See Emacs Help => 24.3 Input Methods and 24.4 Selecting an Input Method
;;; C-\ to select (first time) or toggle thereafter
;;; C-h I METHOD <RET> or
;;; C-h C-\ METHOD <RET> for help

;; With this setup, your workflow becomes completely streamlined:

;; 1. Type C-c f f to review or write French.
;; 2. Hit C-\ to instantly toggle back to standard English for regular typing or coding.
;; 3. Type C-c f s when you want to switch over to Spanish, and your fingers will already know exactly what to do.

(global-set-key (kbd "C-c f f") (lambda () (interactive) (activate-input-method "french-prefix")))
(global-set-key (kbd "C-c f s") (lambda () (interactive) (activate-input-method "spanish-prefix")))
(global-set-key (kbd "C-c f g") (lambda () (interactive) (activate-input-method "german-prefix")))

;;; Emacs has brilliant built-in tools for handling multilingual text.
;;; Emacs Transient Input Methods (Highly Recommended)

;;; Type M-x set-input-method (or use the shortcut C-x Enter C-\)
;;; Type spanish-postfix or spanish-prefix and hit Enter.

;;; If you choose spanish-prefix, typing text becomes incredibly intuitive:
;;; Type ~? => ¿
;;; Type ~! => ¡

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Map Keybindings for Language Accent Input: accent-map
;;; 2026-03-10T22:30
;;; These were tested for conflicts and no conflicts were found.

;; Mnemonics: C-c i stands for Insert or International, making it easy
;; to remember  when you're  in the  middle of  a French  or Icelandic
;; drill.

;; Using a prefix character is a classic Emacs strategy. We can set it
;; up so that if you press the  Shift key with the number (e.g., !, @,
;; #), it triggers the uppercase  version. However, since symbols like
;; ! and # can be tricky to type on some keyboard layouts, I’ve set up
;; the logic below to allow for two intuitive ways to do it:

;; 1. Shifted Numbers: Use the symbol  above the number (e.g., C-c i !
;; e for È).

;; 2. Shifted  Letters: Use the  standard number but a  capital letter
;; (e.g., C-c i 1 E for È).

;; The "Full Coverage" Elisp Snippet
;; This  version covers  both  lowercase and  uppercase  for all  your
;; French, German, and Icelandic needs.

(defun insert-char-fn (char)
  (lambda () (interactive) (insert char)))

(defvar-keymap accent-map
  :doc "Comprehensive map for French, German, and Icelandic with Full Capital support."
  ;; 1: Grave (è, À...)
  "1 e" (insert-char-fn "è") "1 E" (insert-char-fn "È")
  "1 a" (insert-char-fn "à") "1 A" (insert-char-fn "À")
  "1 u" (insert-char-fn "ù") "1 U" (insert-char-fn "Ù")

  ;; 2: Acute (é, Á...)
  "2 e" (insert-char-fn "é") "2 E" (insert-char-fn "É")
  "2 a" (insert-char-fn "á") "2 A" (insert-char-fn "Á")
  "2 i" (insert-char-fn "í") "2 I" (insert-char-fn "Í")
  "2 o" (insert-char-fn "ó") "2 O" (insert-char-fn "Ó")
  "2 u" (insert-char-fn "ú") "2 U" (insert-char-fn "Ú")
  "2 y" (insert-char-fn "ý") "2 Y" (insert-char-fn "Ý")

  ;; 3: Circumflex (ê, Â...)
  "3 e" (insert-char-fn "ê") "3 E" (insert-char-fn "Ê")
  "3 a" (insert-char-fn "â") "3 A" (insert-char-fn "Â")
  "3 i" (insert-char-fn "î") "3 I" (insert-char-fn "Î")
  "3 o" (insert-char-fn "ô") "3 O" (insert-char-fn "Ô")
  "3 u" (insert-char-fn "û") "3 U" (insert-char-fn "Û")

  ;; 4: Umlaut (ë, Ä...)
  "4 a" (insert-char-fn "ä") "4 A" (insert-char-fn "Ä")
  "4 o" (insert-char-fn "ö") "4 O" (insert-char-fn "Ö")
  "4 u" (insert-char-fn "ü") "4 U" (insert-char-fn "Ü")
  "4 e" (insert-char-fn "ë") "4 E" (insert-char-fn "Ë")
  "4 i" (insert-char-fn "ï") "4 I" (insert-char-fn "Ï")

  ;; 5: Specials (ç, ß)
  "5 c" (insert-char-fn "ç") "5 C" (insert-char-fn "Ç")
  "5 s" (insert-char-fn "ß") "5 S" (insert-char-fn "ẞ") ; Capital Eszett added!

  ;; 6: Icelandic (ð, þ, æ)
  "6 d" (insert-char-fn "ð") "6 D" (insert-char-fn "Ð")
  "6 t" (insert-char-fn "þ") "6 T" (insert-char-fn "Þ")
  "6 a" (insert-char-fn "æ") "6 A" (insert-char-fn "Æ"))


(defun my-accent-cheat-sheet ()
  "Reference for custom accents. Use Shift+Letter for Capitals."
  (interactive)
  (let ((buf-name "*Accent-Cheat-Sheet*"))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "⌨️  ACCENT KEYBINDINGS (Prefix: C-c i)\n")
        (insert "-----------------------------------\n")
        (insert "Logic: [Number] + [Letter]. Use SHIFT for CAPS.\n\n")
        (insert "1: Grave (è)   2: Acute (é)    3: Circumflex (ê)\n")
        (insert "4: Umlaut (ä)  5: Specials (ç) 6: Icelandic (ð,þ,æ)\n")
        (insert "-----------------------------------\n")
        (insert "Example: C-c i 2 E -> É | C-c i 6 T -> Þ\n")
        (insert "Press 'q' to close.")
        (read-only-mode 1)
        (local-set-key (kbd "q") 'quit-window)))
    (display-buffer-in-side-window (get-buffer buf-name) '((side . bottom)))))


;; Bind to your safe harbor
(keymap-set global-map "C-c i" accent-map)
(keymap-set accent-map "?" #'my-accent-cheat-sheet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quick-sdcv
;; 2026-06-16
;; Custom Configuration for Multi-Tiered Offline SDCV Lookup

;; C-x D a :: Search All dictionaries simultaneously (the original master fallback).
;; C-x D e :: Search English-only (ready for your upcoming dictionary).
;; C-x D f, C-x D g, C-x D s, C-x D i :: Target your specific foreign languages.

(maybe-require-package 'quick-sdcv)

(with-eval-after-load 'quick-sdcv
  (setq quick-sdcv-data-dir (expand-file-name "~/.stardict/dic"))
  (setq quick-sdcv-open-buffer-function #'display-buffer)
  (define-key quick-sdcv-mode-map (kbd "q") 'quit-window))

(defun my-sdcv-lookup-engine (&optional dict-name)
  "Core engine to look up word at point. If DICT-NAME is provided,
queries that database explicitly; otherwise, searches all databases.
Automatically strips French elision prefixes (e.g., l', d', qu')."
  (let ((word (thing-at-point 'word t)))
    (if (not word)
        (message "No word found at point to look up.")
      ;; Clean up French elisions down to the root word
      (setq word (replace-regexp-in-string
                  (rx (seq string-start
                           (or "l" "d" "j" "m" "t" "s" "n" "c" "qu")
                           (or "'" "’")))
                  "" word))

      (let* ((cmd (if dict-name
                      (format "sdcv -n -u %s %s"
                              (shell-quote-argument dict-name)
                              (shell-quote-argument word))
                    (format "sdcv -n %s" (shell-quote-argument word))))
             (raw-output (shell-command-to-string cmd))
             (output raw-output))

        ;; Parse out the HTML markup structures cleanly
        (setq output (replace-regexp-in-string "<br\\s-*/?>" "\n" output))
        (setq output (replace-regexp-in-string "<li>" "\n • " output))
        (setq output (replace-regexp-in-string "<[^>]+>" "" output))
        (setq output (replace-regexp-in-string "\n\n+" "\n" output))
        (setq output (string-trim output))

        ;; Verify if sdcv returned an actual entry
        (if (string-match-p "Your search found" output)
            (message "No entry found for '%s'%s."
                     word (if dict-name (format " in %s" dict-name) ""))
          (message "%s" output))))))

;; --- Interactive Key Target Commands ---

(defun my-sdcv-lookup-all ()
  "Look up word at point across all active dictionaries simultaneously."
  (interactive)
  (my-sdcv-lookup-engine)) ; No argument defaults to global scan

(defun my-sdcv-lookup-english ()
  "Look up word at point in the English dictionary."
  (interactive)
  ;; Replace the string below with the exact name from 'sdcv -l' once installed
  (my-sdcv-lookup-engine "English Dictionary Placeholder"))

(defun my-sdcv-lookup-french ()
  "Look up French word at point."
  (interactive)
  (my-sdcv-lookup-engine "French-English Wiktionary dictionary (fr-en)"))

(defun my-sdcv-lookup-german ()
  "Look up German word at point."
  (interactive)
  (my-sdcv-lookup-engine "German - English"))

(defun my-sdcv-lookup-swedish ()
  "Look up Swedish word at point."
  (interactive)
  (my-sdcv-lookup-engine "Swedish - English"))

(defun my-sdcv-lookup-icelandic ()
  "Look up Icelandic word at point."
  (interactive)
  (my-sdcv-lookup-engine "Icelandic-English Placeholder"))


;; end quick-scdv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Denote auto writeroom toggle
;; 2026-06-16

;; C-x D w to toggle writeroom for Denote files automatically

(defvar my-auto-writeroom-for-denote-p nil
  "Non-nil means automatically enable `writeroom-mode` when opening Denote files.")

(defun my-denote-writeroom-hook-function ()
  "Enable `writeroom-mode` if the opened file lives inside the Denote directory."
  (when (and my-auto-writeroom-for-denote-p
             (fboundp 'writeroom-mode)
             (fboundp 'denote-directory)
             (string-prefix-p (expand-file-name (denote-directory))
                              (expand-file-name default-directory)))
    (writeroom-mode 1)))

;; Attach the scanner to Emacs' global file-opening engine
(add-hook 'find-file-hook #'my-denote-writeroom-hook-function)

;;;###autoload
(defun my-toggle-denote-writeroom-focus ()
  "Toggle automatic `writeroom-mode` for newly opened Denote files."
  (interactive)
  (setq my-auto-writeroom-for-denote-p (not my-auto-writeroom-for-denote-p))
  (message "Automatic Writeroom for Denote files is now: %s"
           (if my-auto-writeroom-for-denote-p "ENABLED 🧘" "DISABLED 🛑")))

;; end auto writeroom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymapping for writeroom mode and dictionaries
;; 2026-06-16

;; 1. Establish a persistent global keymap for your dictionary/writing sandbox
(defvar my-sandbox-prefix-map (make-sparse-keymap)
  "Keymap for custom offline dictionary and writing workspace commands.")

;; 2. Bind the prefix map globally to the capital 'D' slot under C-x
(global-set-key (kbd "C-x D") my-sandbox-prefix-map)

;; 3. Populate the map directly so these are active immediately at startup
(define-key my-sandbox-prefix-map (kbd "a") 'my-sdcv-lookup-all)
(define-key my-sandbox-prefix-map (kbd "e") 'my-sdcv-lookup-english)
(define-key my-sandbox-prefix-map (kbd "f") 'my-sdcv-lookup-french)
(define-key my-sandbox-prefix-map (kbd "g") 'my-sdcv-lookup-german)
(define-key my-sandbox-prefix-map (kbd "s") 'my-sdcv-lookup-swedish)
(define-key my-sandbox-prefix-map (kbd "i") 'my-sdcv-lookup-icelandic)
(define-key my-sandbox-prefix-map (kbd "w") 'my-toggle-denote-writeroom-focus)

;; end keymapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-local)
;;; init-local.el ends here
