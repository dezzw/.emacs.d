;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error nil)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

;; Enable with t if you prefer
(defconst *spell-check-support-enabled* nil )
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *org-path* "~/Documents/Org/")
(defconst *fallback-fonts* '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst *emoji-fonts* '("Apple Color Emoji"
                          "Noto Color Emoji"
                          "Noto Emoji"
                          "Segoe UI Emoji"
                          "Symbola"))
;; (defconst *default-font* "MonaspiceAr Nerd Font Mono")
(defconst *default-font* "Maple Mono NF")
;; (defconst *zh-default-font* "LXGW WenKai")
(defconst *zh-default-font* "Maple Mono NF CN")
(defconst *symbol-default-font* "Symbols Nerd Font Mono")

(setq user-full-name "Desmond Wang")
(setq user-mail-address "desmond.wang@netint.ca")

(setq vc-follow-symlinks t)

;; load module settings
(dolist (dir '("modules" "lib" "site-lisp" "themes"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'setup)
(require 'init-setup)

(setup (:require keyfreq)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          meow-next
          meow-prev
          execute-extended-command
          vertico-next
          backward-char
          previous-line
          next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(when *is-mac* (require 'init-mac))
(require 'init-ui)

(require 'init-editing)
(require 'init-vc)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-prog)
(require 'init-util)
(require 'init-transient)
(require 'init-bitstream)

(require 'init-org)
(require 'init-reader)
(require 'init-social)

(require 'init-shell)

(require 'init-ai)

(require 'init-local)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
