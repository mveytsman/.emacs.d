;;; Code:

;;; A lot of this was cribbed from
;;; - Giorgio's config https://github.com/ppold/.emacs.d
;;; - Graphene https://github.com/rdallasgray/graphene
;;; - Lee's config http://writequit.org/org/settings.html
;;; I used to keep a fork of Giorgio's config, but I decided to start over from scratch,
;;; keeping his file structure 

;; turn on debug mode during initialization
(setq debug-on-error t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq user-lisp-dir
      (expand-file-name "user-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path (concat user-emacs-directory "core"))
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path user-lisp-dir)



;; Setup packages
(require 'setup-package)

;; Appearance
(require 'appearance)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . , (expand-file-name
                 (concat user-emacs-directory "backups"))))
      auto-save-file-name-transforms
      `((".*" , (expand-file-name
                 (concat user-emacs-directory "autosaves")) t))
      backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Load Mac-only config
(when (equal system-type 'darwin) (require 'mac))

;; Editing
(require 'editing)
(require 'sane-defaults)
(require 'key-bindings)

;; Load user specific configuration
(when (file-exists-p user-lisp-dir)
  (mapc 'load (directory-files user-lisp-dir nil "^[^#].*el$")))

;; Turn debug mode back off
(setq debug-on-error nil)
