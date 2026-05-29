;;; init-shell.el --- Shell and terminal integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup eshell
  (:global-bind "C-c z" 'eshell)
  (:when-loaded
    (:also-load lib-eshell)
    (:option eshell-prompt-function 'eshell-prompt-multiline
             eshell-highlight-prompt nil
             eshell-banner-message ""
             eshell-cmpl-ignore-case t)
    (:with-map eshell-command-mode-map
      (:bind "C-l"  +eshell-clear
             "<tab>" completion-at-point
             "C-c l" +consult-eshell-history))
    (:with-mode eshell-mode
      (:hook (lambda ()
               (+set-eshell-aliases +eshell-aliases)
               (display-line-numbers-mode -1)
               (eshell-cmpl-mode -1)))
      (:hooks eshell-directory-change-hook +sync-dir-in-buffer-name))))

(setup eshell-syntax-highlighting
  (:after eshell
    (eshell-syntax-highlighting-global-mode 1)))

(setup vterm
  (:defer (:require vterm))
  (:autoload project-vterm)
  (setopt vterm-shell "zsh"
          vterm-always-compile-module t)
  (:also-load lib-vterm)
  (:with-map vterm-mode-map
    (:bind "C-y" vterm-yank
           "M-y" vterm-yank-pop
           "C-k" vterm-send-C-k-and-kill)))

(unless *is-mac*
  (setup ghostel
    (:also-load ghostel-eshell)
    (setopt ghostel-shell "zsh"
            ghostel-module-directory user-emacs-directory
            ghostel-tramp-shell-integration t)
    (:hooks eshell-load-hook ghostel-eshell-visual-command-mode)
    (defun dw/ghostel-tramp (host &optional user dir)
      "Open Ghostel directly on HOST as USER using TRAMP."
      (interactive
       (list (read-string "Host: ")
             (read-string "User: " "work")
             (read-string "Remote dir: " "~/")))
      (let ((default-directory
             (format "/ssh:%s@%s:%s" user host (or dir "~/"))))
        (ghostel)))
    (:when-loaded
      (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
      )))

(if (and (not (display-graphic-p))
         (eq (daemonp) "tui"))
    (setup kitty-graphics
      (:hook-into after-init)))

(provide 'init-shell)
;;; init-shell.el ends here
