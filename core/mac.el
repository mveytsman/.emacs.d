;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; full screen binding
(progn
  (global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen))

;; osx keys
(set-keyboard-coding-system nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)



(provide 'mac)
