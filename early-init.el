;;; early-init.el --- early init file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Desmond Wang
;;
;; Author: Desmond Wang <dw@dezzw.com>
;; Maintainer: Desmond Wang <dw@dezzw.com>
;; Created: August 06, 2023
;; Modified: August 06, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/early-init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(when (not (fboundp 'igc-stats))
  (setq gc-cons-threshold most-positive-fixnum))

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)


;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
;; (setq native-comp-jit-compilation nil)

;; change for compile-angel.el
(setq native-comp-jit-compilation nil)
(setq native-comp-async-query-on-exit t)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Without this, Emacs will try to resize itself to a specific column size
(setq frame-inhibit-implied-resize t)

;;; Disable unneeded UI elements

;; Disable startup screens and messages
(setq inhibit-splash-screen t)


;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)

(setq make-backup-files       nil
      auto-save-default       nil
      ring-bell-function      'ignore
      ;; tab-bar-mode 1
      ;; pixel-scroll-precision-mode 1
      )


(if (eq system-type 'darwin)
    (progn
      (setq frame-resize-pixelwise  t)
      (when (display-graphic-p)
	(menu-bar-mode t))))

(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here

