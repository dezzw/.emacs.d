;;; package --- init file -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;
;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
;; (setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
;; (setq auto-mode-case-fold nil)

;; (unless (or (daemonp) noninteractive init-file-debug)
;;   ;; Suppress file handlers operations at startup
;;   ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
;;   (let ((old-value file-name-handler-alist))
;;     (setq file-name-handler-alist nil)
;;     (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
;;     (add-hook 'emacs-startup-hook
;;               (lambda ()
;;                 "Recover file name handlers."
;;                 (setq file-name-handler-alist
;;                       (delete-dups (append file-name-handler-alist old-value))))
;;               101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("modules" "test"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'.
;;    Don't put large files in `site-lisp' directory, e.g. EAF.
;;    Otherwise the startup will be very slow."
;;   (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)a

(update-load-path)

(require 'init-util)
(require 'init-package)

(require 'init-base)

(require 'init-env)

(require 'init-ui)

(require 'init-edit)
(require 'init-helpful)

(require 'init-dired)

(require 'init-term)
(require 'init-org)

(require 'init-hydra)

(require 'init-wm)

;; project management
(require 'init-project)

;; prog
(require 'init-prog)
(require 'init-dap)
;;; lsp config
;; (require 'init-lspbridge)
;; (require 'init-eglot)
(require 'init-lsp)
(require 'init-ctags)
(require 'init-jupyter)
(require 'init-ai)

;;; lang
(require 'init-treesit)
(require 'init-web)
(require 'init-cc)
(require 'init-python)
(require 'init-go)
(require 'init-clojure)
(require 'init-clisp)
(require 'init-gdscript)
(require 'init-markdown)
(require 'init-docker)
(require 'init-haskell)
(require 'init-nix)
(require 'init-swift)
(require 'init-zig)
(require 'init-rust)
(require 'init-tex)
(require 'init-db)
(require 'init-lua)

;; terms
(require 'init-term)

;; magit
(require 'init-magit)

;; social media
(require 'init-telega)

(require 'init-leetcode)

(require 'init-tramp)
(require 'init-telega)

(require 'init-tools)

(require 'init-completion)
