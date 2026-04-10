;;; init-completion.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +orderless-basic-all (str table pred point)
  "Fallback from Orderless to basic completion for STR.
TABLE, PRED, and POINT are forwarded to the completion backends."
  (or (orderless-all-completions str table pred point)
      (completion-basic-all-completions str table pred point)))

(defun +orderless-basic-try (str table pred point)
  "Fallback from basic completion to Orderless for STR.
TABLE, PRED, and POINT are forwarded to the completion backends."
  (or (completion-basic-try-completion str table pred point)
      (orderless-try-completion str table pred point)))

(setup orderless
  (:require orderless)
  (:when-loaded
    (:option completion-styles '(orderless flex)
             completion-ignore-case t
             ;; https://github.com/minad/corfu/issues/136
             ;; eglot 会更改 completion-category-defaults 这个变量。
             ;; 需要通过修改 completion-category-overrides 改为 orderless
             completion-category-overrides '((file (styles partial-completion basic)))
             orderless-component-separator "[ &]")
    (add-to-list 'completion-styles-alist
                 '(orderless+basic
                   +orderless-basic-try
                   +orderless-basic-all
                   "Unholy mix of Orderless and Basic."))))

(setup corfu
  (:defer (:require corfu))
  (:when-loaded
    (setopt corfu-cycle t
            corfu-auto t
            corfu-auto-prefix 2
            corfu-preselect 'prompt
            corfu-quit-no-match 'separator)
    (global-corfu-mode)
    (:with-feature nerd-icons-corfu
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
    (:with-mode corfu
      (:bind "<right>" corfu-quit
             "TAB" corfu-next
             [tab] corfu-next
             "S-TAB" corfu-previous
             [backtab] corfu-previous))
    (:with-mode eshell-mode
      (:hook (lambda () (setq-local corfu-auto nil))))
    (:with-feature meow
      (add-hook 'meow-insert-mode-hook #'corfu-quit))))

(setup cape
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(setup yasnippet
  (:option yas-verbosity 0)
  (:with-hook after-init-hook
    (:hook yas-global-mode))
  (add-hook 'yas-keymap-disable-hook
            (lambda () (and (frame-live-p corfu--frame)
                            (frame-visible-p corfu--frame)))))

(provide 'init-completion)
;;; init-completion.el ends here
