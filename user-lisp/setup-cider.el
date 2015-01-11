(require 'cider)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(setq cider-show-error-buffer 'except-in-repl)

;;(require 'popwin)
;;(push '("*cider-error*" :width 60 :position right) popwin:special-display-config)

(provide 'setup-cider)
