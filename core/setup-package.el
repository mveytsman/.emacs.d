; Set up packaging system
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
;        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; Set up pallet for package management
(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(provide 'setup-package)
