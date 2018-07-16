;; Stolen from https://github.com/arecker/emacs.d/
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;(setq debug-on-error 't
;      network-security-level 'low)

(defun mveytsman/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(mveytsman/package-init)

;; Configuration
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Themes
(use-package color-theme
  :ensure t)
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  (toggle-frame-fullscreen))
  
;; Path
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;(setq inhibit-startup-message 't)
(use-package unkillable-scratch
  :ensure t
  :init (unkillable-scratch))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(projects . 5)))
  :init
  (dashboard-setup-startup-hook))

;; Start the server if it's not running
(require 'server)
(unless (server-running-p)
  (server-start))

;; Interface

;; Better defaults
(setq make-backup-files nil
      auto-save-default nil
      indent-tabs-mode nil
      ns-confirm-quit 1)

(global-auto-revert-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode t)

;; Better comments
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))
;; Hide all minor modes from the modeline (since there are usually like a
(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :config (setq rm-blacklist ""))

;; Better File Manager

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; Better Text Selection
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Better Completion
(use-package company
  :ensure t
  :config (global-company-mode))

;; Completion and filtering with ivy, supported by counsel.


(use-package ivy
  :ensure t
  :config (setq ivy-use-selectable-prompt t)
  :init (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind
  ("C-c i" . counsel-imenu)
  ("C-c s" . swiper)
  ("C-c g" . counsel-git-grep)
  ("C-x C-y" . counsel-yank-pop))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

;; Magit

(use-package magit
  :ensure t
  :bind
  ("C-c m" . magit-status)
  ("C-c b" . magit-blame)
  :config (magit-add-section-hook 'magit-status-sections-hook
				  'magit-insert-unpushed-to-upstream
				  'magit-insert-unpushed-to-upstream-or-recent
				  'replace))

;; Modes

;; Projectile
(use-package projectile
  :ensure t
  :bind (;("C-p s" . projectile-switch-open-project)
	 ("C-x p" . projectile-switch-project))
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))
;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package ag
  :ensure t)

;; Cleanup whitespace on save
(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode))

;; Elisp
(use-package slime
  :ensure t
  :config (setq inferior-lisp-program (executable-find "sbcl")))

(use-package slime-company
  :ensure t
  :init (slime-setup '(slime-fancy slime-company)))
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
;; Csv

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

;; Dockerfile

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")


;; Go
(use-package go-mode
  :ensure t
  :mode "\\*.go\\'")


;; Javascript
(setq js-indent-level 2)


;; Terminal
(defun mveytsman/ansi-term ()
  (interactive)
  (ansi-term "/usr/local/bin/fish"))
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x t") 'mveytsman/ansi-term)

;; Ruby
(use-package ruby-mode
  :ensure t
  :mode "\\*.rb\\'"
  :config 
  (setq inf-ruby-console-patterns-alist
	'((inf-ruby-console-script-p . script)
	  (".zeus.sock" . zeus)
	  (inf-ruby-console-rails-p . rails)
	  (inf-ruby-console-hanami-p . hanami)
	  ("*.gemspec" . gem)
	  (inf-ruby-console-racksh-p . racksh)
	  ("Gemfile" . default))))

(use-package projectile-rails
  :ensure t
  :config (projectile-rails-global-mode))

(use-package robe
  :ensure t
  :bind ("C-M-." . robe-jump)

  :init
  (add-hook 'ruby-mode-hook 'robe-mode))
;; Set safe local variables so I can do local
(setq safe-local-variable-values
      '((projectile-rails-vanilla-command . "bin/rails")
	(projectile-rails-spring-command . "bin/spring")
	(projectile-rails-zeus-command . "bin/zeus")
	(flycheck-ruby-rubocop-executable . "bin/rubocop")))


;; Org Mode

(use-package org
  :ensure t
  :config (org-babel-do-load-languages
	   'org-babel-load-languages
	   '((awk . t)
	     (C . t)
	     (calc . t)
	     (clojure . t)
	     (css . t)
	     (ditaa . t)
	     (ditaa . t)
	     (haskell . t)
	     (java . t)
	     (js . t)
	     (latex . t)
	     (lisp . t)
	     (makefile . t)
	     (perl . t)
	     (python . t)
	     (ruby . t)
	     ;; (scala . t)
	     (screen . t)
	     ;; (sh . t)
	     (sql . t)
	     (sqlite . t))))
