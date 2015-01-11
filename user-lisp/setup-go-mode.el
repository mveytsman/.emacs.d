(require 'go-mode)

(setenv "GOPATH" "/Users/maxim/projects/go/")

(add-to-list 'exec-path "/Users/maxim/projects/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)

(setq gofmt-command "goimports")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;;unused imports
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)

  ; setup autocompletion
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; autocompletion
(setq company-go-show-annotation t)
(setq company-go-insert-arguments nil)

;; graphene
;;(add-to-list 'graphene-prog-mode-hooks 'go-mode)
                                        

(require 'company-go)

(provide 'setup-go-mode)
