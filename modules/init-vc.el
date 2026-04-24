;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup ediff-wind
  (:option ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(setup smerge-mode
  (:hooks prog-mode-hook smerge-mode))

(setup autorevert
  (:option global-auto-revert-non-file-buffers t
           auto-revert-verbose nil)
  (:hooks after-init-hook global-auto-revert-mode))

(setup magit
  (:global-bind "C-x g" 'magit-status
                "C-x M-g" 'magit-dispatch)
  (:also-load lib-magit)
  (:with-map magit-status-mode-map
    (:bind
     "C-M-<up>" magit-section-up
     ;; Work around a Transient/Magit discard bug on Emacs 31 by
     ;; providing a direct entry point that bypasses dispatch popup "k".
     "C-c C-k" magit-discard))
  (:with-map vc-prefix-map
    (:bind "l" +magit-or-vc-log-file
           ;; file binding for vc-git-grep
           "f" vc-git-grep))
  ;; 将当前 view 的 buffer 写入文件，实现恢复以前版本的作用
  (:with-map magit-blob-mode-map
    (:bind "C-c C-c" +magit-blob-save
           "C-n"     magit-blob-next
           "C-p"     magit-blob-previous))
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
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

(setup magit-log
  (:after magit
    ;; Show commit ages with 1-char time units (m/h/d/w/M/Y)
    ;; Reduce author column width to 11 as name is abbreviated
    (:option magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
    (:advice magit-log-format-margin :filter-args #'+magit-log--abbreviate-author)))

(setup diff-hl
  (:option diff-hl-update-async t)
  (:hooks prog-mode-hook diff-hl-mode
          conf-mode-hook diff-hl-mode
          dired-mode-hook diff-hl-dired-mode))

(setup blame-reveal
  (:set blame-reveal-recent-days-limit 'auto
        blame-reveal-gradient-quality 'auto
        blame-reveal-show-uncommitted-fringe nil)
  (:set blame-reveal-async-blame 'auto)
  (:when-loaded
    (require 'blame-reveal-recursive)

    (defun dw/blame-reveal--abbreviate-author (author)
      "Abbreviate AUTHOR name."
      (cond
       ((string-match "\\(.\\).*?, *\\(.*\\)" author)
        (replace-match "\\2 \\1" nil nil author))
       ((string-match "\\(.*?\\)[. ]+\\(.\\).*" author)
        (replace-match "\\1 \\2" nil nil author))
       (t author)))
    (advice-add 'blame-reveal--abbreviate-author :override #'dw/blame-reveal--abbreviate-author)
    ))

(provide 'init-vc)
;;; init-vc.el ends here
