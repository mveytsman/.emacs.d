;; Stolen from https://github.com/arecker/emacs.d/
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

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

;; Update packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

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
(setq-default make-backup-files nil
      auto-save-default nil
      indent-tabs-mode nil
      ns-confirm-quit 1)

(global-auto-revert-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode t)

;; Bettewr window navigation
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

;; Better comments
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))
;; Hide all minor modes from the modeline (since there are usually like a
(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :config (setq rm-blacklist ""))

;; better replace
(use-package visual-regexp
  :ensure t
  :bind ("C-c r" . vr/replace))

;; Show lines when goto-line invoked
(defun mveytsman/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

(global-set-key [remap goto-line] 'mveytsman/goto-line-with-feedback)

;; Better File Manager

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; Better Text Selection
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Quickly open init file
(defun mveytsman/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c f") 'mveytsman/find-user-init-file)

;; Better Completion
(use-package company
  :ensure t
  :config (global-company-mode))

;; Completion and filtering with ivy, supported by counsel w/ amx.
(use-package ivy
  :ensure t
  :config (setq ivy-use-selectable-prompt t)
  :init (ivy-mode 1))

(use-package ivy-hydra
  :ensure t)

(use-package counsel
  :ensure t
  :bind
  ("C-c i" . counsel-imenu)
  ("C-s" . swiper)
  ("C-x C-y" . counsel-yank-pop)
  ("M-x" . counsel-M-x)
  ("C-c M-x" . amx-major-mode-commands)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-h b" . counsel-descbinds)
  ("C-h C-b" . describe-bindings))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

(use-package amx
  :ensure t)

;; Magit

(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-unpushed-to-upstream
			  'magit-insert-unpushed-to-upstream-or-recent
			  'replace))

(use-package libgit
  :ensure t
  :after magit)
;; Modes

;; Projectile
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (;("C-p s" . projectile-switch-open-project)
	 ("C-x p" . projectile-switch-project))
  :config
  (projectile-mode)
  (setq projectile-keymap-prefix (kbd "C-c P"))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

;; Neotree
(use-package neotree
  :ensure t
  :bind ("C-c t" . neotree-toggle))

;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package ag
  :ensure t)

;; Cleanup whitespace on save
(use-package ws-butler
 :ensure t)

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
(setq css-indent-offset 2)

;; YAML
(use-package yaml-mode
  :ensure t)
;; Web mode
(use-package web-mode
  :ensure t
  :mode "\\.html$"
  :bind ("C-c b" . web-beautify-html)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(use-package web-beautify
  :ensure t
  :bind (:map web-mode-map
              ("C-c b" . web-beautify-html)))

;; Terminal
(defun mveytsman/ansi-term ()
  (interactive)
  (ansi-term "/usr/local/bin/fish"))

(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x t") 'mveytsman/ansi-term)

(use-package shell-pop
  :ensure t
  :config
  (custom-set-variables
   '(shell-pop-default-directory "~/")
   '(shell-pop-shell-type
     (quote ("eshell" "*eshell*" (lambda nil (mveytsman/ansi-term)))))
   '(shell-pop-universal-key "C-'")
   '(shell-pop-window-height 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))

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

;; Elixir
(use-package elixir-mode
  :ensure t
  :init (add-hook 'elixir-format-hook (lambda ()
                                 (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                   (setq elixir-format-arguments nil)))))
(use-package alchemist
  :ensure t
  :config
  (setq alchemist-goto-erlang-source-dir "/usr/local/opt/asdf/installs/erlang/21.0.3/")
  (setq alchemist-goto-elixir-source-dir "/usr/local/opt/asdf/installs/elixir/1.6.6/"))

(use-package flycheck-credo
  :ensure t
  :init (add-hook 'elixir-mode-hook 'flycheck-credo-setup))

;; Clojure
(use-package cider
  :ensure t
  :bind ("C-c M-j" . cider-jack-in))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

;; Org Mode

(use-package org
  :ensure t
  :config
  (setq org-startup-indented 't
        org-startup-folded nil)

  (org-babel-do-load-languages
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

;; Deft mode
(use-package deft
  :ensure t
  :bind ("C-c q" . deft)
  :config (setq deft-directory "~/Dropbox/org"
                deft-default-extension "org"))
