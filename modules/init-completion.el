;;; init-completion.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup orderless
  (:defer (:require orderless))
  (:when-loaded
    (:option completion-styles '(orderless flex)
             completion-category-defaults nil
             completion-ignore-case t
             ;; https://github.com/minad/corfu/issues/136
             ;; eglot 会更改 completion-category-defaults 这个变量。
             ;; 需要通过修改 completion-category-overrides 改为 orderless
             completion-category-overrides '((file (styles partial-completion basic)))
             orderless-component-separator "[ &]")
    ;; pinyinlib.el 用于匹配简体/繁体汉字拼音首字母
    (add-to-list 'orderless-matching-styles
                 (lambda (str)
                   (orderless-regexp
                    (pinyinlib-build-regexp-string str))))

    ;; https://github.com/oantolin/orderless/issues/111#issuecomment-1098763842
    (defun orderless+basic-all (str table pred point)
      (or (orderless-all-completions str table pred point)
          (completion-basic-all-completions str table pred point)))

    (defun orderless+basic-try (str table pred point)
      (or (completion-basic-try-completion str table pred point)
          (orderless-try-completion str table pred point)))

    (add-to-list 'completion-styles-alist
                 '(orderless+basic
                   orderless+basic-try
                   orderless+basic-all
                   "Unholy mix of Orderless and Basic."))))


(setup corfu
  (:defer (:require corfu))
  (:when-loaded
    ;; Make lsp-proxy compatible with nerd-icons-corfu
    (with-eval-after-load 'lsp-proxy-completion
      (defun lsp-proxy--candidate-kind-normalize (orig-fun &rest args)
        "Convert lsp-proxy kind strings to symbols for nerd-icons-corfu compatibility."
        (when-let* ((kind-string (apply orig-fun args)))
          (let ((result (intern
                         (pcase kind-string
                           ("Text" "text")
                           ("Method" "method")
                           ("Function" "function")
                           ("Constructor" "constructor")
                           ("Field" "field")
                           ("Variable" "variable")
                           ("Class" "class")
                           ("Interface" "interface")
                           ("Module" "module")
                           ("Property" "property")
                           ("Unit" "unit")
                           ("Value" "value")
                           ("Enum" "enum")
                           ("Keyword" "keyword")
                           ("Snippet" "snippet")
                           ("Color" "color")
                           ("File" "file")
                           ("Reference" "reference")
                           ("Folder" "folder")
                           ("EnumMember" "enum-member")
                           ("Constant" "constant")
                           ("Struct" "struct")
                           ("Event" "event")
                           ("Operator" "operator")
                           ("TypeParameter" "type-parameter")
                           (_ (downcase kind-string))))))
            result)))
      ;; Remove old advice if it exists, then add the new one
      (advice-remove 'lsp-proxy--candidate-kind #'lsp-proxy--candidate-kind-normalize)
      (advice-add 'lsp-proxy--candidate-kind :around #'lsp-proxy--candidate-kind-normalize)
      (message "lsp-proxy-corfu: Advice installed for nerd-icons compatibility"))

    (:with-feature nerd-icons-corfu
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
    (global-corfu-mode)
    (:option corfu-cycle t
             corfu-auto t
             corfu-auto-prefix 2
             corfu-quit-no-match 'separator)
    (:with-mode prog-mode (:hook corfu-mode))
    (:with-mode corfu
      (:bind "<right>" corfu-quit
             "TAB"  corfu-next
             [tab]  corfu-next
             "S-TAB"  corfu-previous
             [backtab]  corfu-previous))
    (:with-mode eshell-mode
      (:local-set corfu-auto nil)
      (corfu-mode))
    (:with-feature meow
      (add-hook 'meow-insert-mode-hook 'corfu-quit))))

(setup cape
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'completion-at-point-functions #'cape-emoji)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(setup yasnippet
  (:defer (:require yasnippet))
  (:when-loaded
    (yas-global-mode)
    (add-hook 'yas-keymap-disable-hook
              (lambda () (and (frame-live-p corfu--frame)
                              (frame-visible-p corfu--frame))))
    (setq yas-verbosity 0)))


(provide 'init-completion)
;;; init-completion.el ends here
