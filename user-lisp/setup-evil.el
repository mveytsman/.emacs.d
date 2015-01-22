 (require 'evil)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-visualstar)

(evilnc-default-hotkeys)
;; these modes are clear from evil
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'prodigy-mode-hook 'evil-emacs-state)
(add-hook 'cider-repl-mode-hook 'evil-emacs-state)

;; evil messes up emacs undo
(global-undo-tree-mode 0)


;; Copy and paste
(define-key evil-normal-state-map (kbd "s-c") 'evil-yank)
(define-key evil-normal-state-map (kbd "s-p") 'evil-paste-after)


(add-to-list 'evil-emacs-state-modes 'cider-error-buffer)


;; I prefer having jump-to-tag bound to M-.
(define-key evil-normal-state-map (kbd "M-.") nil)
;; this does something wonky to closing popup buffers
(define-key evil-normal-state-map (kbd "q") nil)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)
(evil-mode 1)
(provide 'setup-evil)
