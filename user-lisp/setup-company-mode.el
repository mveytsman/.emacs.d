(require 'company)

(defun company-complete-common-or-selection ()
  "Insert the common part of all candidates, or the selection."
  (interactive)
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (let ((company-selection-wrap-around t))
          (call-interactively 'company-complete-selection))))))



(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "ESC") 'company-abort)
(define-key company-active-map [tab] 'company-complete-common-or-selection)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-selection)
(setq company-idle-delay 0.125
      company-minimum-prefix-length 3
      company-require-match nil
      company-transformers '(company-sort-by-occurrence)
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
			  company-preview-frontend
			  company-echo-metadata-frontend))

;; I like my tab completion 



(add-hook 'prog-mode-hook 'company-mode)

(provide 'setup-company-mode)
