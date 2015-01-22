
;;; Code:

(setq visible-bell nil
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil
      ring-bell-function (lambda()))

;; Set custom theme path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; Highlight current line
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (setq-default line-spacing 2)
  (add-to-list 'default-frame-alist '(internal-border-width . 0))
  (set-fringe-mode '(8 . 0))
;;  (set-frame-font "ProggyClean")
;  (set-face-attribute 'default nil :family "Prog" :height 140)
  (set-face-font 'default "DejaVu Sans Mono-12"))
(add-to-list 'default-frame-alist '(font . "-*-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
                                        ;(load-theme 'zenburn t)
(load-theme 'solarized t)
(load-theme 'graphene t)


(setq initial-scratch-message "")

;; Get rid of alarms
(setq ring-bell-function 'ignore)

(provide 'appearance)
