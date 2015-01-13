(require 'flyspell)
;; find aspell and hunspell automatically
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_CA")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setq ispell-extra-args '("-d en_CA")))
 )

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;flyspell overrides a keybinding I want globally
(define-key flyspell-mode-map (kbd "C-;") nil)
(provide 'setup-flyspell)
