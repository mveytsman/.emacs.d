(require 'cider)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'clojure-mode-hook #'company-mode)

(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'subword-mode)


(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(setq cider-show-error-buffer 'except-in-repl)

;;(require 'popwin)
;;(push '("*cider-error*" :width 60 :position right) popwin:special-display-config)

; remap up arrow
(define-key cider-repl-mode-map (kbd "s-<up>") 'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "s-<down>") 'cider-repl-next-input)


(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")

(provide 'setup-cider)

