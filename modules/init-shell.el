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
    (:with-map eshell-mode-map
      (:bind "C-l"  +eshell-clear
             "<tab>" completion-at-point
             "C-c l" +consult-eshell-history))
    (:with-mode eshell-mode
      (:hook (lambda ()
               (+set-eshell-aliases +eshell-aliases)
               (display-line-numbers-mode -1)
               (eshell-cmpl-mode -1)))
      (:hooks eshell-directory-change-hook +sync-dir-in-buffer-name)))
  (:with-hook eshell-load-hook
    (:hook eat-eshell-mode)
    (:hook eat-eshell-visual-command-mode)))

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

(setup ghostel
  (:autoload ghostel ghostel-other ghostel-download-module)
  (setopt ghostel-shell "zsh")
  (:when-loaded
    (:also-load 'ghostel-eshell)
    (add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode)
    ))

(unless (display-graphic-p)
  (setup kitty-graphics
    (:hook-into after-init)))

(provide 'init-shell)
;;; init-shell.el ends here
