;;; graphene-speedbar.el --- Graphene defaults for Speedbar
;;
;; Copyright (c) @YEAR Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: @VERSION
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; This file defines default settings and functionality for the Speedbar.
;; Graphene by default uses sr-speedbar by Sebastian Rose.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
;;(graphene-unpin-speedbar)
(require 'sr-speedbar)
(autoload 'sr-speedbar-open "sr-speedbar" "Open the in-frame speedbar" t)

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      ;speedbar-file-unshown-regexp "flycheck-.*"
      sr-speedbar-width 30
      sr-speedbar-width-x 30
      sr-speedbar-auto-refresh nil
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

(add-hook 'speedbar-mode-hook
          '(lambda ()
             (hl-line-mode 1)
             (visual-line-mode -1)
             (setq automatic-hscrolling nil)
             (let ((speedbar-display-table (make-display-table)))
               (set-display-table-slot speedbar-display-table 0 8230)
               (setq buffer-display-table speedbar-display-table))))
;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)
             (define-key speedbar-mode-map (kbd "TAB") 'speedbar-toggle-line-expansion)))


(define-key speedbar-mode-map (kbd "TAB") 'speedbar-toggle-line-expansion)


;; Always use the last selected window for loading files from speedbar.
(defvar last-selected-window
  (if (not (eq (selected-window) sr-speedbar-window))
      (selected-window)
    (other-window 1)))

(defadvice select-window (after remember-selected-window activate)
  "Remember the last selected window."
  (unless (or (eq (selected-window) sr-speedbar-window)
              (not (window-live-p (selected-window))))
    (setq last-selected-window (selected-window))))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hooks `speedbar-before-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hooks `speedbar-before-visiting-tag-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hooks `speedbar-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hooks `speedbar-visiting-tag-hook'."
  (select-window last-selected-window))


(defun sr-speedbar-refresh-projectile-root ()
  "Refreshes the speedbar in the projectile root"
  (interactive)
  (when (not (sr-speedbar-window-p))
    (let ((project-root
         (condition-case nil
             (projectile-project-root)
           (error default-directory))))
      (sr-speedbar-select-window)
      (setq default-directory project-root)
      (speedbar-refresh))))

(speedbar-add-supported-extension ".go")
(speedbar-add-supported-extension ".clj")
(provide 'graphene-speedbar)
