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


(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Determine the state of bundled libraries using calc-loaddefs.el.
     ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
     ;; If compiled or neither, omit the gzip handler during startup for
     ;; improved startup and package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Ensure the new value persists through any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Remember the old value to reset it as needed.
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist old-value))))
              101))

  (unless noninteractive
    (progn
      ;; Disable mode-line-format during init
      (defun minimal-emacs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))

      (defvar minimal-emacs--default-mode-line-format mode-line-format
        "Default value of `mode-line-format'.")
      (setq-default mode-line-format nil)

      (defun my--startup-load-user-init-file (fn &rest args)
        "Around advice for startup--load-user-init-file to reset mode-line-format."
        (let (init)
          (unwind-protect
              (progn
                (apply fn args)  ; Start up as normal
                (setq init t))
            (unless init
              ;; If we don't undo inhibit-{message, redisplay} and there's an
              ;; error, we'll see nothing but a blank Emacs frame.
              (minimal-emacs--reset-inhibited-vars-h))
            (unless (default-toplevel-value 'mode-line-format)
              (setq-default mode-line-format
                            minimal-emacs--default-mode-line-format)))))

      (advice-add 'startup--load-user-init-file :around
                  #'my--startup-load-user-init-file))

    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'lisp-interaction-mode
          initial-scratch-message nil)))

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

