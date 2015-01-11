(require 'helper-functions)

;;Straight from graphene
(global-set-key (kbd "C-x k")
                'kill-default-buffer)
(global-set-key (kbd "C-x C-k")
                'kill-buffer-and-window)
(global-set-key (kbd "C-c n")
                'create-new-buffer)
(global-set-key (kbd "C-c N")
                'new-emacs-instance)
(global-set-key (kbd "C-;")
                'insert-semicolon-at-end-of-line)
(global-set-key (kbd "M-RET")
                'newline-anywhere)
(global-set-key (kbd "C-M-;")
                'comment-current-line-dwim)
(global-set-key (kbd "C->")
                'increase-window-height)
(global-set-key (kbd "C-<")
                'decrease-window-height)
(global-set-key (kbd "C-,")
                'decrease-window-width)
(global-set-key (kbd "C-.")
                'increase-window-width)
(global-set-key (kbd "M-x")
                'smex)
(global-set-key (kbd "M-X")
                'smex-major-mode-commands)
(global-set-key (kbd "C-c a")
                'sr-speedbar-toggle)
(global-set-key (kbd "C-c s")
                'sr-speedbar-select-window)
(global-set-key (kbd "C-c d")
                'my-speedbar-toggle-updates)

;;Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Helpfule for jumping around elisp
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

;; Jump to buffer after splits
(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Multi term
(global-set-key (kbd "C-'") 'multi-term-dedicated-toggle)
(global-set-key (kbd "C-c t") 'multi-term)
(global-set-key (kbd "M-]") 'multi-term-next)
(global-set-key (kbd "M-[") 'multi-term-prev)

;; I still like some readline bindings, not sure if this is best way to undo evil
(global-set-key [remap evil-scroll-line-down] 'move-end-of-line)
(global-set-key [remap evil-copy-from-below] 'move-end-of-line)
(global-set-key [remap evil-insert-digraph] 'kill-line)

(global-set-key (kbd "C-c q") 'deft)


(provide 'key-bindings)
