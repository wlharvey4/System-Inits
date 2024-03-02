;;; updates.el -- Code to Update init files
;;; Time-stamp: <2024-02-25 11:38:24 minilolh>

;;; Commentary:
;;  Notes on how to modify purcell-emacs.d for my local machines

;;; Code:

;;; early-init.el
;; Create early maximized gui screen
;; Add
(push '(fullscreen . maximized) default-frame-alist)

;;; init.el
;; Allow access from emacsclient "guiserver"
;; Modify
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (when (display-graphic-p)
              (unless (server-running-p)
                (setq server-name "guiserver")
                (server-start)
                (setenv "GUISERVER" "RUNNING")))))

;;; init-local.el
;; should be symlinked into purcell-emacs.d/lisp

;;; end updated.el
