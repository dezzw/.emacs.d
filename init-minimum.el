;;; init-minimum.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

(defconst *spell-check-support-enabled* nil ) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *org-path* "~/Documents/Org/")
(defconst *fallback-fonts* '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst *emoji-fonts* '("Apple Color Emoji"
                          "Noto Color Emoji"
                          "Noto Emoji"
                          "Segoe UI Emoji"
                          "Symbola"))
(defconst *default-font* "MonaspiceAr Nerd Font Mono")
(defconst *org-font* "MonaspiceAr Nerd Font Mono")
(defconst *term-default-font* "MonaspiceAr Nerd Font Mono")
(defconst *prog-font* "MonaspiceAr Nerd Font Mono")
(defconst *zh-default-font* "LXGW WenKai")
(defconst *jp-default-font* "Noto Sans Javanese")
(defconst *symbol-default-font* "Symbols Nerd Font Mono")

;; Install straight.el
;; branch develop
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install packages
(defvar *use-package-list*
  '(
    setup
    (emt :host github :repo "roife/emt")
    ;; ==== Put the packages related to the code below this line! ====
    ))

(dolist (e *use-package-list*)
  (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(require 'init-setup)
(when *is-mac* (require 'init-mac))
;; ==== put your code below this line! ====
;; emacs -Q -l ~/.emacs.d/init-minimum.el
;;; init-minimum.el ends here

(when window-system
  (setup faces
    (:also-load lib-face)
    (:hooks window-setup-hook +setup-fonts
            server-after-make-frame-hook +setup-fonts
            default-text-scale-mode-hook +setup-fonts
            after-make-frame-functions +setup-fonts)))
