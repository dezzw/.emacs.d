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

;; All packages are now managed by Nix via flake.nix
;; No need for straight.el bootstrap or package installation here
;; Package management has been moved to NixOS for reproducibility
(setq vc-follow-symlinks t)

;; load module settings
(dolist (dir '("modules" "lib" "site-lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'init-setup)
(when *is-mac* (require 'init-mac))
(require 'init-ui)

(require 'init-editing)
(require 'init-vc)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-prog)
(require 'init-util)
(require 'init-transient)

(require 'init-org)
(require 'init-reader)

(require 'init-shell)

(require 'init-ai)

(require 'init-local)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
