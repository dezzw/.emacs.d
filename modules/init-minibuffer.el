;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup (:with-hook after-init-hook
         (:hook savehist-mode)
         (:hook mode-line-bell-mode)))

(setup recentf
  (:hook-into after-init)
  (:when-loaded
    (:option recentf-max-saved-items 50
             recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                   "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                   "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                   (lambda (file) (file-in-directory-p file package-user-dir))
                                   (expand-file-name recentf-save-file))
             recentf-keep nil)
    ;; Add dired directories to recentf file list.
    (:with-mode dired-mode
      (:hook (lambda () (recentf-add-file default-directory))))
    (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
    ;; HACK: Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))

(setup minibuffer
  ;; 用于对补全候选项进行分类的变量。通过将它们设置为 nil，我们禁用了 Emacs 自动分类补全候选项的功能，从而获得更简洁的补全列表。
  (:option completion-category-defaults nil
           completion-category-overrides nil
           ;; 将阈值设置为 4 表示只有当需要补全的字符数大于 4 时才会执行循环补全
           completion-cycle-threshold 4))

(setup awesome-tray
  (:defer (:require awesome-tray))
  (:option awesome-tray-active-modules '("meow" "file-path" "mode-name" "git" "flymake"))
  (:when-loaded
    (awesome-tray-mode 1)))

(setup vertico
  (:defer (:require vertico))
  (:when-loaded
    (:option vertico-cycle t)
    (:with-map vertico-map
      (:bind
       "RET" vertico-directory-enter
       "DEL" vertico-directory-delete-char
       "M-DEL" vertico-directory-delete-word))
    (vertico-mode)))

(setup vertico-posframe
  (:load-after vertico)
  (:require vertico-posframe)
  (:when-loaded
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

(setup consult
  (:defer (:require consult))
  (:when-loaded
    (keymap-global-set "C-c f l" 'consult-line)
    (keymap-global-set "C-c f i" 'consult-imenu)
    (keymap-global-set "C-c f f" 'consult-fd)
    (keymap-global-set "C-c f r" 'consult-ripfd)
    (keymap-global-set "C-c f g" 'consult-goto-line)
    (keymap-global-set "C-c f p" 'consult-project-buffer)
    (keymap-global-set "C-c f b" 'consult-buffer)
    (keymap-global-set "C-c f d" 'consult-flymake)
    (keymap-global-set "C-c f m" 'consult-global-mark)
    (keymap-global-set "<remap> <switch-to-buffer>" 'consult-buffer)
    (keymap-global-set "<remap> <switch-to-buffer-other-window>" 'consult-buffer-other-window)
    (keymap-global-set "<remap> <switch-to-buffer-other-frame>" 'consult-buffer-other-frame)
    (keymap-global-set "<remap> <goto-line>" 'consult-goto-line)
    (:also-load lib-consult)
    (:option consult-async-min-input 2
             xref-show-xrefs-function #'consult-xref
             xref-show-definitions-function #'consult-xref)
    (:hooks minibuffer-setup-hook mcfly-time-travel)))

(setup consult-dir
  (:load-after vertico)
  (:when-loaded
    (keymap-global-set "C-x C-d" 'consult-dir)
    (:with-map vertico-map
      (:bind
       "C-x C-d" consult-dir
       "C-x C-j" consult-dir-jump-file))))

(setup isearch
  (:option isearch-lazy-count t
           isearch-allow-motion t
           isearch-motion-changes-direction t))

(setup embark
  (:defer (:require embark))
  (:when-loaded
    (:also-load embark-consult)

    (defun +embark-open-in-finder (file)
      "Open FILE in macOS Finder."
      (interactive "fFile: ")
      (shell-command (format "open -R %s && osascript -e 'tell application \"Finder\" to activate'" (shell-quote-argument (expand-file-name file)))))
    
    (keymap-global-set "C-c ." 'embark-act)
    (keymap-global-set "M-n"   'embark-next-symbol)
    (keymap-global-set "M-p"   'embark-previous-symbol)
    (:with-map embark-file-map (when *is-mac* (:bind "o" +embark-open-in-finder)))
    (:option embark-indicators '(embark-minimal-indicator
                                 embark-highlight-indicator
                                 embark-isearch-highlight-indicator)
             embark-cycle-key "."
             embark-help-key "?")
    (:hooks embark-collect-mode-hook consult-preview-at-point-mode)))

(setup marginalia
  (:load-after vertico)
  (:when-loaded
    (marginalia-mode)))

(setup nerd-icons-completion
  (:load-after vertico)
  (:when-loaded (nerd-icons-completion-mode)))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
