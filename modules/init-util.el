;;; init-util.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup files
  (:option  auto-save-default nil
            auto-save-visited-interval 1.1
            auto-save-visited-predicate
            (lambda () (and (not (buffer-live-p (get-buffer " *vundo tree*")))
                            (not (string-suffix-p "gpg" (file-name-extension (buffer-name)) t))
                            (not (eq (buffer-base-buffer (get-buffer (concat "CAPTURE-" (buffer-name))))
                                     (current-buffer)))
                            (or (not (boundp 'corfu--total)) (zerop corfu--total))
                            (or (not (boundp 'yas--active-snippets)) (not yas--active-snippets))))
            make-backup-files nil
            enable-local-variables :all
            ;; emacs@31 feature
            trusted-content '("~/.emacs.d/")))

(setup dired
  (:defer (:require dired))
  (:when-loaded
    (:with-map ctl-x-map (:bind "\C-j" 'dired-jump))
    (:with-map ctl-x-4-map (:bind "\C-j" 'dired-jump-other-window))
    (:option dired-recursive-deletes 'top
             dired-dwim-target t
             dired-recursive-copies 'always
             dired-kill-when-opening-new-dired-buffer t)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))
    (:with-mode dired-mode (:hook diff-hl-dired-mode
                                  dired-hide-details-mode
                                  nerd-icons-dired-mode
                                  diredfl-mode))))

(setup bookmark
  (:defer
   (:option bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))))

(setup dirvish
  (:defer (:require dirvish))
  (:when-loaded
    (:global "C-c f f" dirvish
             "C-c f s" dirvish-side)
    (dirvish-override-dired-mode)
    (:option dirvish-quick-access-entries
             '(("h" "~/" "Home")
               ("e" "~/.emacs.d/" "Emacs")
               ("p" "~/Library/CloudStorage/BeeStation-MyBeeStation/Projects/" "Projects"))
             dirvish--debouncing-delay 2
             dirvish-side-width 50
             dirvish-attributes '(file-time file-size collapse subtree-state vc-state)
             dirvish-side-attributes '(vc-state collapse)
             delete-by-moving-to-trash t
             dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
             dirvish-mode-line-height 15
             dirvish-header-line-height '(15 .25))
    (:with-map dirvish-mode-map
      (:bind "a"    dirvish-quick-access
             "q"    dirvish-quit
             "f"    dirvish-file-info-menu
             "y"    dirvish-yank-menu
             "N"    dirvish-narrow
             "h"    dirvish-history-jump
             "s"    dirvish-quicksort
             "TAB"  dirvish-subtree-toggle
             "M-f"  dirvish-history-go-forward
             "M-b"  dirvish-history-go-backward
             "M-l"  dirvish-ls-switches-menu
             "M-m"  dirvish-mark-menu
             "M-t"  dirvish-layout-toggle
             "M-s"  dirvish-setup-menu
             "M-e"  dirvish-emerge-menu
             "M-j"  dirvish-fd-jump))
    (:with-mode dirvish-directory-view-mode (:hook diredfl-mode))))

(setup helpful
  (:defer (:require helpful))
  (:global
   [remap describe-function] helpful-function
   [remap describe-symbol] helpful-symbol
   [remap describe-variable] helpful-variable
   [remap describe-command] helpful-command
   [remap describe-key] helpful-key))

(setup leetcode
  (:autoload leetcode-list-all)
  (:option leetcode-language "swift"))

(provide 'init-util)
;;; init-util.el ends here
