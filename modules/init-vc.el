;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup ediff-wind
  (:option ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(add-hook 'prog-mode-hook (function smerge-mode))

(setup autorevert
  (:defer (:require autorevert))
  (:when-loaded
    (:option  global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)
    (global-auto-revert-mode)
    ;; 隐藏一些比较冗长的 mode 名称，从而让 mode-line 更加简洁。
    (diminish 'auto-revert-mode)))

(setup magit
  (:when-loaded
    (:also-load lib-magit)
    (:with-map magit-status-mode-map
      (:bind "C-M-<up>" magit-section-up))
    (:with-map vc-prefix-map
      (:bind "l" +magit-or-vc-log-file
             ;; file binding for vc-git-grep
             "f" vc-git-grep))
    ;; 将当前 view 的 buffer 写入文件，实现恢复以前版本的作用
    (:with-map magit-blob-mode-map
      (:bind "C-c C-c" +magit-blob-save
             "C-n"     magit-blob-next
             "C-p"     magit-blob-previous))
    ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
    ;; quickly open magit on any one of your projects.
    (keymap-global-set "C-x g" 'magit-status)
    (keymap-global-set "C-x M-g" 'magit-dispatch)
    (:option magit-diff-refine-hunk t
             ;; Don't autosave repo buffers. This is too magical, and saving can
             ;; trigger a bunch of unwanted side-effects, like save hooks and
             ;; formatters. Trust the user to know what they're doing.
             magit-save-repository-buffers nil
             ;; Don't display parent/related refs in commit buffers; they are rarely
             ;; helpful and only add to runtime costs.
             magit-revision-insert-related-refs nil
             magit-blame-styles '((headings
                                   (heading-format . "  %C %-18a%f %-80s  %H\n")
                                   (show-message . t))
                                  (highlight
                                   (highlight-face . magit-blame-highlight))))
    (:advice magit-status :around #'magit-fullscreen)
    (:advice magit-mode-quit-window :after #'magit-restore-screen)
    ;; kill 因为 blob-next 和 blob-previous 产生的 buffer
    (:advice magit-blob-next :around #'kill-all-blob-next-after-quit)
    (:advice magit-blob-previous :around #'kill-all-blob-previous-after-quit)
    (when *is-mac*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))))

(setup magit-log
  (:load-after magit)
  (:when-loaded
    ;; Set `magit-log-margin' value in :init as many other variables will be
    ;; dynamically set based on its value when `magit-log' is loaded.
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    ;; Show the commit ages with 1-char time units
    ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
    ;; Also reduce the author column width to 11 as the author name is being
    ;; abbreviated below.
    (:option magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
    (:advice magit-log-format-margin :filter-args #'+magit-log--abbreviate-author)))

(setup forge
  (:load-after magit)
  (:when-loaded
    ;; Make it easier to see that a topic was closed.
    (:face forge-topic-closed ((t (:strike-through t))))
    (push '("git.netint.ca"               ; GITHOST
            "git.netint.ca/api/v4"        ; APIHOST
            "git.netint.ca"               ; WEBHOST and INSTANCE-ID
            forge-gitlab-repository)    ; CLASS
          forge-alist)))

(setup diff-hl
  (:defer (diff-hl-mode))
  (:when-loaded
    (:option diff-hl-update-async t)
    (:hooks prog-mode-hook diff-hl-mode
            conf-mode-hook diff-hl-mode
            dired-mode-hook diff-hl-dired-mode)
    (:with-map diff-hl-mode-map
      (:bind "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk))))

;; (setup sideline-blame
;;   (:defer (:require sideline-blame))
;;   (:option sideline-backends-right '((sideline-blame . up)))
;;   (:when-loaded
;;     (global-sideline-mode 1)))

(setup blame-reveal
  (:defer (:require blame-reveal))
  (:when-loaded
    (setq blame-reveal-recent-days-limit 'auto)        ; Adapt to commit frequency
    (setq blame-reveal-gradient-quality 'auto)         ; Balanced quality
    
    ;; Display
    (setq blame-reveal-display-layout 'compact)        ; Show commit info
    (setq blame-reveal-show-uncommitted-fringe nil)    ; Use diff-hl instead

    ;; Performance
    (setq blame-reveal-async-blame 'auto)              ; Async for large files

    ;; Enable recursive blame
    (require 'blame-reveal-recursive)))

(provide 'init-vc)
;;; init-vc.el ends here
