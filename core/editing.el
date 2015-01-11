;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Linum format to avoid graphics glitches in fringe
(setq linum-format " %4d ")

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)



;; Main hook to be run on entering de facto prog modes, enabling linum, flycheck,
;; electric-indent, and smartparens
(add-hook 'prog-mode-hook
          (lambda ()
              (linum-mode t)
              (company-mode)
;	      (flyspell-prog-mode)
	      (flycheck-mode)
	      (electric-indent-mode 1)
	      (show-paren-mode nil)
	      (setq blink-matching-paren nil)
	      (setq sp-show-pair-delay 0)
	      (show-smartparens-mode)))
(provide 'editing)
