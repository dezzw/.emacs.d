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

(setup consult-omni
  (:load-after consult)
  (:option consult-omni-show-preview t ;;; show previews
           consult-omni-preview-key "C-o"
           consult-omni-highlight-matches-in-minibuffer t ;;; highlight matches in minibuffer
           consult-omni-highlight-matches-in-file t ;;; highlight matches in files
           consult-omni-default-count 5 ;;; set default count
           consult-omni-default-page 0
           consult-omni-dynamic-input-debounce 0.8
           consult-omni-dynamic-input-throttle 1.6
           consult-omni-dynamic-refresh-delay 0.8) ;;; set the preview key to C-o
  (:when-loaded
    ;; Load Sources Core code
    (require 'consult-omni-sources)
    ;; Load Embark Actions
    (require 'consult-omni-embark)

    ;; Either load all source modules or a selected list

    ;; Select a list of modules you want to aload, otherwise all sources all laoded
    (setq consult-omni-sources-modules-to-load (list
                                                'consult-omni-apps
                                                'consult-omni-brave-autosuggest
                                                'consult-omni-brave
                                                'consult-omni-browser-history
                                                'consult-omni-buffer
                                                'consult-omni-calc
                                                'consult-omni-consult-notes
                                                'consult-omni-dict
                                                'consult-omni-fd
                                                'consult-omni-find
                                                'consult-omni-gh
                                                'consult-omni-git-grep
                                                'consult-omni-google
                                                'consult-omni-google-autosuggest
                                                ;; 'consult-omni-gptel
                                                'consult-omni-grep
                                                ;; 'consult-omni-invidious
                                                'consult-omni-line-multi
                                                'consult-omni-locate
                                                'consult-omni-man
                                                'consult-omni-mdfind
                                                'consult-omni-org-agenda
                                                'consult-omni-projects
                                                'consult-omni-ripgrep
                                                'consult-omni-ripgrep-all
                                                'consult-omni-stackoverflow
                                                'consult-omni-wikipedia
                                                'consult-omni-youtube
                                                ))

    (consult-omni-sources-load-modules)

    ;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
    (setq consult-omni-multi-sources '("calc"
                                       ;; "File"
                                       ;; "Buffer"
                                       ;; "Bookmark"
                                       "Apps"
                                       ;; "gptel"
                                       ;; "Brave"
                                       "Dictionary"
                                       "Google"
                                       "Wikipedia"
                                       ;; "buffers text search"
                                       "Notes Search"
                                       "Org Agenda"
                                       "GitHub"
                                       "YouTube"))

    ;; Per source customization

    ;; Set API KEYs. It is recommended to use a function that returns the string for better security.
    (setq consult-omni-google-customsearch-key "AIzaSyAcg1-rn_7FFO_QRMBbQgM0c2Tz_OgnzNI")
    (setq consult-omni-google-customsearch-cx "068b46c41739e4141")
    (setq consult-omni-brave-api-key "BSA3oU11HpInkmKaS3-Gk4_iy9l7Tyk")
    (setq consult-omni-stackexchange-api-key "YOUR-STACKEXCHANGE-API-KEY-OR-FUNCTION")

    ;; gptel settings
    ;; (setq consult-omni-gptel-cand-title #'consult-omni--gptel-make-title-short-answer)

    ;; default terminal
    (setq consult-omni-embark-default-term #'vterm)

    ;; default video player
    ;; (setq consult-omni-embark-video-default-player  #'mpv-play-url)

    ;; pretty prompt for launcher
    (setq consult-omni-open-with-prompt "  ")

    ;; Pick your favorite autosuggest command.
    (setq consult-omni-default-autosuggest-command #'consult-omni-dynamic-google-autosuggest) ;;or any other autosuggest source you define

    ;; Set your shorthand favorite interactive command
    (setq consult-omni-default-interactive-command #'consult-omni-multi)

    ;; Optionally Set back-end for notes search to ripgrep-all (requires ripgrep-all)
    ;; (setq consult-omni-notes-backend-command "rga")

    ;; Optionally add more interactive commands

    ;; consult-omni-web
    (defvar consult-omni-web-sources (list "Google"
                                           "Wikipedia"
                                           "GitHub"
                                           "YouTube"
                                           ))
    (defun consult-omni-web (&optional initial prompt sources no-callback &rest args)
      "Interactive web search”

This is similar to `consult-omni-multi', but runs the search on
web sources defined in `consult-omni-web-sources'.
See `consult-omni-multi' for more details.
"
      (interactive "P")
      (let ((prompt (or prompt (concat "[" (propertize "consult-omni-web" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
            (sources (or sources consult-omni-web-sources)))
        (consult-omni-multi initial prompt sources no-callback args)))

    ;; consult-omni-local
    (defvar consult-omni-local-sources (list "ripgrep"
                                             "mdfind"
                                             "Notes Search"
                                             "Apps"
                                             "Org Agenda"))
    (defun consult-omni-local (&optional initial prompt sources no-callback &rest args)
      "Interactive local search”

This is similar to `consult-omni-multi', but runs the search on
local sources defined in `consult-omni-local-sources'.
See `consult-omni-multi' for more details.
"
      (interactive "P")
      (let ((prompt (or prompt (concat "[" (propertize "consult-omni-local" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
            (sources (or sources consult-omni-local-sources)))
        (consult-omni-multi initial prompt sources no-callback args)))

    ;; AutoSuggest at point
    (defun consult-omni-autosuggest-at-point ()
      (interactive)
      (let ((input (or (thing-at-point 'url) (thing-at-point 'filename) (thing-at-point 'symbol) (thing-at-point 'sexp) (thing-at-point 'word))))
        (when (and (minibuffer-window-active-p (selected-window))
                   (equal (substring input 0 1) (consult--async-split-initial nil)))
          (setq input (substring input 1)))
        (consult-omni-brave-autosuggest input))))
  )

(provide 'init-util)
;;; init-util.el ends here
