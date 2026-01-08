;;; init-shell.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup vterm
  (:autoload project-vterm)
  (:when-loaded
    (defun vterm-send-C-k-and-kill ()
      "Send `C-k' to libvterm, and put content in kill-ring."
      (interactive)
      (kill-ring-save (point) (vterm-end-of-line))
      (vterm-send-key "k" nil nil t))

    (defun project-vterm ()
      "Create or switch to a vterm buffer for the current project.
Similar to `project-eshell', each project gets its own vterm buffer."
      (interactive)
      (require 'project)
      (let* ((default-directory (project-root (project-current t)))
             (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
             (vterm-buffer (get-buffer default-project-vterm-name)))
        (unless vterm-buffer
          (setq vterm-buffer (generate-new-buffer default-project-vterm-name))
          (with-current-buffer vterm-buffer
            (vterm-mode)))
        (pop-to-buffer vterm-buffer)))

    (:with-map vterm-mode-map
      (:bind "C-y" vterm-yank
             "M-y" vterm-yank-pop
             "C-k" vterm-send-C-k-and-kill))
    (:option vterm-shell "zsh"
             vterm-always-compile-module t)))

(setup esh-mode
  (keymap-global-set "<f8>" 'eshell)
  (:when-loaded
    (:require eshell)
    (:also-load esh-mode)    
    (:also-load lib-eshell)
    (:also-load nerd-icons)
    (:also-load eat)
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
               (+set-eshell-aliases +aliases)
               (display-line-numbers-mode -1)
               (eshell-cmpl-mode -1)))
      (:hooks eshell-directory-change-hook +sync-dir-in-buffer-name)))
   (:with-hook eshell-load-hook
    (:hook eat-eshell-mode)
    (:hook eat-eshell-visual-command-mode)))

(setup eshell-syntax-highlighting
  (:load-after esh-mode)
  (:when-loaded (eshell-syntax-highlighting-global-mode +1)))

(provide 'init-shell)
;;; init-shell.el ends here
