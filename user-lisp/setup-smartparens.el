(require 'smartparens)
(require 'smartparens-config)

(defun sp-pair-on-newline (id action context)
  "Put trailing pair on newline and return to point."
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun sp-pair-on-newline-and-indent (id action context)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (sp-pair-on-newline id action context)
  (indent-according-to-mode))
  
(sp-pair "{" nil :post-handlers
         '(:add ((lambda (id action context)
                   (sp-pair-on-newline-and-indent id action context)) "RET")))
(sp-pair "[" nil :post-handlers
         '(:add ((lambda (id action context)
                   (sp-pair-on-newline-and-indent id action context)) "RET")))

(sp-local-pair '(markdown-mode gfm-mode) "*" "*"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

(dolist (mode '(coffee-mode shell-mode))
  (add-to-list 'sp-autoescape-string-quote-if-empty mode))

(setq sp-highlight-pair-overlay nil)
;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
;; to customize/view this list.
(mapc (lambda (mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'rainbow-delimiters-mode))
      sp--lisp-modes)

;; Conveniently set keys into the sp-keymap, limiting the keybinding to buffers
;; with SP mode activated
(mapc (lambda (info)
        (let ((key (kbd (car info)))
              (function (car (cdr info))))
          (define-key sp-keymap key function)))
      '(("C-M-f" sp-forward-sexp)
        ("C-M-b" sp-backward-sexp)
 
        ("C-M-d" sp-down-sexp)
        ("C-M-a" sp-backward-down-sexp)
        ("C-S-a" sp-beginning-of-sexp)
        ("C-S-d" sp-end-of-sexp)
 
        ("C-M-e" sp-up-sexp)
 
        ("C-M-u" sp-backward-up-sexp)
        ("C-M-t" sp-transpose-sexp)
 
        ("C-M-n" sp-next-sexp)
        ("C-M-p" sp-previous-sexp)
 
        ("C-M-k" sp-kill-sexp)
        ("C-M-w" sp-copy-sexp)
 
        ("M-<delete>" sp-unwrap-sexp)
        ("M-<backspace>" sp-backward-unwrap-sexp)
 
        ("C-<right>" sp-forward-slurp-sexp)
        ("C-<left>" sp-forward-barf-sexp)
        ("C-M-<left>" sp-backward-slurp-sexp)
        ("C-M-<right>" sp-backward-barf-sexp)
 
        ("M-D" sp-splice-sexp)
        ("C-M-<delete>" sp-splice-sexp-killing-forward)
        ("C-M-<backspace>" sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" sp-splice-sexp-killing-around)
 
        ("C-]" sp-select-next-thing-exchange)
        ("C-<left_bracket>" sp-select-previous-thing)
        ("C-M-]" sp-select-next-thing)
 
        ("M-F" sp-forward-symbol)
        ("M-B" sp-backward-symbol)
 
        ("H-t" sp-prefix-tag-object)
        ("H-p" sp-prefix-pair-object)
        ("H-s c" sp-convolute-sexp)
        ("H-s a" sp-absorb-sexp)
        ("H-s e" sp-emit-sexp)
        ("H-s p" sp-add-to-previous-sexp)
        ("H-s n" sp-add-to-next-sexp)
        ("H-s j" sp-join-sexp)
        ("H-s s" sp-split-sexp)))
 
;; This is from authors config, seems to let you jump to the end of the current
;; sexp with paren?
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(provide 'setup-smartparens)
