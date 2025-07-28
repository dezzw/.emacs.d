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
    (:with-feature nerd-icons-corfu
      ;; Using VS Code icons as an alternative
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
    (global-corfu-mode)
    (:option corfu-cycle t
             corfu-auto t
             corfu-auto-prefix 2
             corfu-quit-no-match 'separator)
    (:with-mode prog-mode (:hook corfu-mode))
    (:with-mode corfu
      (:bind "<escape>" corfu-quit
             "<right>" corfu-quit
             "TAB"  corfu-next
             [tab]  corfu-next
             "S-TAB"  corfu-previous
             [backtab]  corfu-previous))
    (:with-mode eshell-mode
      (:local-set corfu-auto nil)
      (corfu-mode))))

;; (setup kind-icon
;;   (:load-after corfu)
;;   (:when-loaded
;;     (add-to-list 'corfu-margin-formatters
;;                  #'kind-icon-margin-formatter)
;;     (advice-add 'reapply-themes :after 'kind-icon-reset-cache)))

(setup cape
  (:load-after corfu)
  (:when-loaded
    (when (or window-system (daemonp))
      (add-to-list 'completion-at-point-functions #'cape-emoji))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(setup yasnippet
  (:defer (:require yasnippet))
  (:when-loaded
    (yas-global-mode)
    (:option yas-keymap-disable-hook
             (lambda () (and (frame-live-p corfu--frame)
                             (frame-visible-p corfu--frame)))
             yas-verbosity 0)))

(setup lsp-mode
  (:defer (:require lsp-mode))
  (:also-load lib-lsp)
  (:option lsp-enable-folding nil
           lsp-enable-text-document-color nil
           lsp-enable-on-type-formatting nil
           lsp-headerline-breadcrumb-enable nil
           lsp-completion-provider :none
           lsp-enable-snippet nil
           lsp-semantic-tokens-enable t
           lsp-enable-indentation nil
           lsp-idle-delay 0.500
           lsp-keymap-prefix "C-x L")
  (:with-mode python-ts-mode
    (:hook
     lsp-inlay-hints-mode
     (lambda ()
       (require 'lsp-pyright)
       (setq lsp-disabled-clients '(pylsp mspyls ruff))
       (setq lsp-pyright-langserver-command "basedpyright")
       (lsp-deferred))))

  (:with-mode java-ts-mode
    (:hook
     (lambda ()
       (require 'lsp-java)
       (lsp-deferred))))

  (:with-mode swift-mode
    (:hook
     (lambda ()
       (require 'lsp-sourcekit)
       (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
       (lsp-deferred))))
  (:when-loaded
    (:with-map lsp-mode-map
      (:bind "M-<return>" lsp-execute-code-action))
    (:hook lsp-lens-mode)
    (:with-mode lsp-completion-mode
      (:hook (lambda ()
               (setf (alist-get 'lsp-capf completion-category-defaults)
                     '((styles . (orderless flex)))))))

    (:with-mode lsp-tailwindcss-major-modes
      (:hook
       (lambda ()
         (setq lsp-tailwindcss-add-on-mode t))))

    (:with-mode (clojure-mode clojurescript-mode clojurec-mode)
      (:hook
       (lambda ()
         (aggressive-indent-mode)
         (setq-local completion-at-point-functions
                     (list (cape-capf-super #'cider-complete-at-point #'lsp-completion-at-point)))
         (lsp-deferred))))

    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

(setup lsp-ui
  (:load-after lsp-mode)
  (:option lsp-ui-doc-max-height 8
           lsp-ui-doc-max-width 72         ; 150 (default) is too wide
           lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
           ;; lsp-ui doc
           lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
           lsp-ui-doc-show-with-cursor t
           ;; lsp-ui sideline
           lsp-ui-sideline-show-hover nil
           lsp-ui-sideline-show-code-actions nil
           ;; lsp signature
           lsp-signature-render-documentation nil)

  (:with-map lsp-ui-mode-map
    (:bind
     [remap xref-find-definitions] lsp-ui-peek-find-definitions
     [remap xref-find-references] lsp-ui-peek-find-references)))

(provide 'init-completion)
;;; init-completion.el ends here
