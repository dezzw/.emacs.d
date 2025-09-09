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
            trusted-content '("~/.emacs.d/"))
  (:hooks after-init-hook auto-save-visited-mode))

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

(when *is-mac*
  (setup jinx
    (:hooks emacs-startup-hook global-jinx-mode)
    (:when-loaded
      (:bind
       "M-$" jinx-correct
       "C-M-$" jinx-languages)

      ;; See issue https://github.com/minad/jinx/issues/4
      ;; This is the syntax table approach. It changes CJK characters from "w" (
      ;; word constituent) to "_" (symbol constituent). You can use `describe-char'
      ;; to view a characters' specific syntax category (from major mode syntax table).
      ;; Emacs 29 supports Unicode 15, the code charts of which can be found at
      ;; http://www.unicode.org/charts/ (use mouse hover to show the specific range)
      (let ((st jinx--syntax-table))
        (modify-syntax-entry '(#x4E00 . #x9FFF) "_" st)   ; CJK Unified Ideographs
        (modify-syntax-entry '(#x3400 . #x4DBF) "_" st)   ; CJK Unified Ideographs Extension A
        (modify-syntax-entry '(#x20000 . #x2A6DF) "_" st) ; CJK Unified Ideographs Extension B
        (modify-syntax-entry '(#x2A700 . #x2B73F) "_" st) ; CJK Unified Ideographs Extension C
        (modify-syntax-entry '(#x2B740 . #x2B81F) "_" st) ; CJK Unified Ideographs Extension D
        (modify-syntax-entry '(#x2B820 . #x2CEAF) "_" st) ; CJK Unified Ideographs Extension E
        (modify-syntax-entry '(#x2CEB0 . #x2EBEF) "_" st) ; CJK Unified Ideographs Extension F
        (modify-syntax-entry '(#x30000 . #x3134F) "_" st) ; CJK Unified Ideographs Extension G
        (modify-syntax-entry '(#x31350 . #x323AF) "_" st) ; CJK Unified Ideographs Extension H
        (modify-syntax-entry '(#x2EBF0 . #x2EE5F) "_" st) ; CJK Unified Ideographs Extension I
        ))))

(setup gptel
  (:pkg gptel)
  (:option gptel-model "openrouter/auto")
  (:when-loaded
    (defun read-file-contents (file-path)
      "Read the contents of FILE-PATH and return it as a string."
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))
    (setq gptel-backend
          (gptel-make-openai "NETINT"
            :host "cursor.netint.ca/openrouter/v1"
            :endpoint "/chat/completions"
            :stream t
            :key (auth-source-pick-first-password :host "cursor.netint.ca" :user "netint")
            :models '("openrouter/auto" "openai/gpt-5-chat" "anthropic/claude-sonnet-4" "anthropic/claude-3.7-sonnet")
            ))))

(setup elysium
  (:pkg elysium)
  (:option
   elysium-window-size 0.33
   elysium-window-style 'vertical))

(provide 'init-util)
;;; init-util.el ends here
