;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup (:with-hook after-init-hook
         (:hook savehist-mode)))

(setup recentf
  (:option recentf-max-saved-items 50
           recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                 "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                 "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                 (lambda (file) (file-in-directory-p file package-user-dir))
                                 (expand-file-name recentf-save-file))
           recentf-keep nil)
  ;; Add dired directories to recentf file list once recentf is active.
  (:with-mode dired-mode
    (:hook (lambda ()
             (when (bound-and-true-p recentf-mode)
               (recentf-add-file default-directory)))))
  (:after recentf
    (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
    ;; HACK: Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties))
  (:hooks after-init-hook recentf-mode))

(setup minibuffer
  ;; 用于对补全候选项进行分类的变量。通过将它们设置为 nil，我们禁用了 Emacs 自动分类补全候选项的功能，从而获得更简洁的补全列表。
  (:option completion-category-defaults nil
           completion-category-overrides nil
           ;; 将阈值设置为 4 表示只有当需要补全的字符数大于 4 时才会执行循环补全
           completion-cycle-threshold 4))

;; (setup miniline
;;   (:require miniline miniline-segments)
;;   (:option
;;    ;; Default format (buffer-local mode-line-format takes priority)
;;    miniline-format miniline-format-default

;;    ;; Position: right-aligned
;;    miniline-position 'right

;;    ;; Hide the original mode-line, show thin separator in GUI
;;    miniline-hide-mode-line t
;;    miniline-display-gui-line t

;;    ;; Update interval in seconds
;;    miniline-update-interval 0.5

;;    ;; Right padding to avoid text wrapping
;;    miniline-right-padding 1)

;;   (:when-loaded
;;     (miniline-mode 1)))

(setup isearch
  (:option isearch-lazy-count t
           isearch-allow-motion t
           isearch-motion-changes-direction t))

(setup doom-modeline
  (:set doom-modeline-height 18
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-modification-icon t
        doom-modeline-project-name t
        doom-modeline-bar-width 4
        doom-modeline-hud t
        doom-modeline-hud-min-height 1)
  (:after doom-modeline
    (:with-feature telega
      (:when-loaded
        (add-to-list 'global-mode-string '("" (:eval (+mode-line-telega-icon))) t))))
  (:defer (doom-modeline-mode 1)))

(setup vertico
  (:option vertico-cycle t)
  (:after vertico
    (:with-map vertico-map
      (:bind
       "RET" vertico-directory-enter
       "DEL" vertico-directory-delete-char
       "M-DEL" vertico-directory-delete-word)))
  (:defer (vertico-mode 1)))

(setup vertico-posframe
  (:after vertico
    (setq vertico-multiform-commands
          '((consult-line
             posframe
             (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
             (vertico-posframe-border-width . 10)
             ;; NOTE: This is useful when emacs is used in both in X and
             ;; terminal, for posframe do not work well in terminal, so
             ;; vertico-buffer-mode will be used as fallback at the
             ;; moment.
             (vertico-posframe-fallback-mode . vertico-buffer-mode))
            (t posframe)))
    (vertico-posframe-mode 1)))

(setup marginalia
  (:after vertico
    (marginalia-mode 1)))

(setup nerd-icons-completion
  (:after vertico
    (nerd-icons-completion-mode 1)))

(setup consult
  (:global-bind "C-c f l" 'consult-line
                "C-c f i" 'consult-imenu
                "C-c f f" 'consult-fd
                "C-c f r" 'consult-ripfd
                "C-c f g" 'consult-goto-line
                "C-c f p" 'consult-project-buffer
                "C-c f b" 'consult-buffer
                "C-c f d" 'consult-flymake
                "C-c f m" 'consult-global-mark
                "<remap> <switch-to-buffer>" 'consult-buffer
                "<remap> <switch-to-buffer-other-window>" 'consult-buffer-other-window
                "<remap> <switch-to-buffer-other-frame>" 'consult-buffer-other-frame
                "<remap> <goto-line>" 'consult-goto-line)
  (:option consult-async-min-input 2
           xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)
  (:hooks minibuffer-setup-hook mcfly-time-travel)
  (:after consult
    (:also-load consult-ripfd)
    (:also-load lib-consult)))

(setup consult-dir
  (:global-bind "C-x C-d" 'consult-dir)
  (:after vertico
    (:after consult-dir
      (:with-map vertico-map
      (:bind
       "C-x C-d" consult-dir
       "C-x C-j" consult-dir-jump-file)))))

(defun +embark-open-in-finder (file)
  "Open FILE in macOS Finder."
  (interactive "fFile: ")
  (shell-command (format "open -R %s && osascript -e 'tell application \"Finder\" to activate'"
                         (shell-quote-argument (expand-file-name file)))))

(setup embark
  (:global-bind "C-c ." 'embark-act
                "M-n"   'embark-next-symbol
                "M-p"   'embark-previous-symbol)
  (:option embark-indicators '(embark-minimal-indicator
                               embark-highlight-indicator
                               embark-isearch-highlight-indicator)
           embark-cycle-key "."
           embark-help-key "?")
  (:hooks embark-collect-mode-hook consult-preview-at-point-mode)
  (:after embark
    (:also-load embark-consult)
    (:with-map embark-file-map (when *is-mac* (:bind "o" +embark-open-in-finder)))))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
