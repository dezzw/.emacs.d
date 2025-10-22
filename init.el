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

;; Install straight.el
;; branch develop
(setq straight-repository-branch "develop")
(setq straight-check-for-modifications '(check-on-save find-when-checking))
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
  '(setup
       nov sis plz avy mpv cape wgrep  nerd-icons
       corfu vundo forge verb elfeed popper embark dimmer vertico
       diredfl separedit cdlatex consult mmm-mode scratch
       diff-hl goggles web-mode js2-mode move-dup diminish
       doom-modeline git-link apheleia pdf-tools ox-pandoc
       macrostep json-mode orderless kind-icon git-modes git-blamed
       ace-pinyin marginalia rainbow-mode prettier-js
       ;;vterm 
       vterm-toggle language-detection meow-tree-sitter
       markdown-mode mode-line-bell embark-consult speed-type
       typescript-mode nerd-icons-dired command-log-mode
       browse-kill-ring rainbow-delimiters default-text-scale denote
       nerd-icons-corfu nerd-icons-completion whitespace-cleanup-mode
       eshell-syntax-highlighting consult-dir dirvish swift-mode
       color-theme-sanityinc-tomorrow highlight-parentheses

       ;;; org related
       org-modern org-appear org-remark org-tidy org-cliplink org-download
       visual-fill-column valign ob-async
       (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
       denote-org denote-markdown

       ;;; language related
       ;;; Clojure
       clojure-ts-mode cider babashka neil
       ;;; LaTeX
       auctex
       fennel-mode nix-ts-mode
       ;;; Scheme
       geiser-chez

       ;;; utilities
       zoom activities citre
       jinx envrc helpful aggressive-indent-mode
       (image-slicing :host github :repo "ginqi7/image-slicing")
       (emt :host github :repo "roife/emt")
       (meow :host github :repo "meow-edit/meow")
       (gptel :host github :repo "karthink/gptel")
       (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
       (telega :host github :repo "LuciusChen/telega.el")
       (md :host github :repo "eki3z/md")
       ;; (telega :host github :repo "zevlg/telega.el")
       (yasnippet :host github :repo "joaotavora/yasnippet")
       (panel :host github :repo "LuciusChen/panel")
       (rose-pine :host github :repo "LuciusChen/rose-pine")
       (indent-bars :host github :repo "jdtsmith/indent-bars")
       (vertico-posframe :host github :repo "tumashu/vertico-posframe")
       (copilot-chat :host github :repo "chep/copilot-chat.el")
       (copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
       (leetcode-emacs :type git :host github :repo "ginqi7/leetcode-emacs")
       ;; (tabspaces :type git :host github :repo "mclear-tools/tabspaces")
       (eat :type git :host codeberg :repo "akib/emacs-eat"
            :files ("*.el" ("term" "term/*.el") "*.texi"
                    "*.ti" ("terminfo/e" "terminfo/e/*")
                    ("terminfo/65" "terminfo/65/*")
                    ("integration" "integration/*")
                    (:exclude ".dir-locals.el" "*-tests.el")))

       ;;; consult-omni
       (consult-omni :type git :host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
       browser-hist consult-notes consult-gh
       ))

(dolist (e *use-package-list*) (straight-use-package e))
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
