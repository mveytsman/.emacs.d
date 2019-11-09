(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (visual-regexp-steroids csv company-lsp toml-mode lsp-ui lsp-mode racer rainbow-delimiters flycheck-rust gradle-mode enh-ruby-mode rust-mode vue-mode ws-butler ethan-wspace ehtan-wspace ghub+ evil-magit yaml-mode ivy-hydra smartparens smart-parens web-beautify auto-package-update web-mode visual-regexp amx popwin smex cider markdown-mode ace-window shell-pop flycheck-credo flycheck-elixir-credo alchemist elixir-mode deft libgit neotree robe projectile-rails go-mode dockerfile-mode csv-mode slime-company slime ag flycheck magit counsel-projectile counsel ivy company expand-region rich-minority comment-dwim-2 dashboard unkillable-scratch exec-path-from-shell use-package)))
 '(safe-local-variable-values
   (quote
    ((flycheck-command-wrapper-function lambda
					(command)
					(append
					 (quote
					  ("bundle" "exec"))
					 command))
     (eval setq flycheck-command-wrapper-function
	   (lambda
	     (command)
	     (append
	      (quote
	       ("bundle" "exec"))
	      command)))
     (projectile-rails-vanilla-command . "bin/rails")
     (projectile-rails-spring-command . "bin/spring")
     (projectile-rails-zeus-command . "bin/zeus")
     (flycheck-ruby-rubocop-executable . "bin/rubocop"))))
 '(shell-pop-default-directory "~/")
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("eshell" "*eshell*"
     (lambda nil
       (mveytsman/ansi-term)))))
 '(shell-pop-universal-key "C-'")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
