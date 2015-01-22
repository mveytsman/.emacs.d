
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


                                        ;(load-theme 'zenburn t)
(load-theme 'solarized-dark t)
(load-theme 'graphene t)


(setq initial-scratch-message "")

;; Get rid of alarms
(setq ring-bell-function 'ignore)

(provide 'appearance)
