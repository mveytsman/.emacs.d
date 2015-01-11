;;; Code:
(require 'flycheck)
(defun flycheck-display-errors-function (errors)
  (mapc (lambda (err)
          (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
        errors))
(setq flycheck-highlighting-mode nil
      flycheck-display-errors-function 'flycheck-display-errors-function)

(provide 'setup-flycheck)
