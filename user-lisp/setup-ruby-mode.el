(require 'chruby)
(chruby "ruby-2.2.1")

(eval-after-load 'company
  '(push 'company-robe company-backends))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (chruby-use-corresponding))

(setq inf-ruby-default-implementation "pry")

(add-hook 'ruby-mode-hook 'robe-mode)


(provide 'setup-ruby-mode)
