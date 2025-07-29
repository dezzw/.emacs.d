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

(setup yasnippet
  (:defer (:require yasnippet))
  (:when-loaded
    (yas-global-mode)
    (:option yas-verbosity 0)))

;; (setup lsp-mode
;;   (:defer (:require lsp-mode))
;;   (:also-load lib-lsp)
;;   (:option lsp-enable-folding nil
;;            lsp-enable-text-document-color nil
;;            lsp-enable-on-type-formatting nil
;;            lsp-headerline-breadcrumb-enable nil
;;            lsp-completion-provider :none
;;            lsp-enable-snippet nil
;;            lsp-semantic-tokens-enable t
;;            lsp-enable-indentation nil
;;            lsp-idle-delay 0.500
;;            lsp-keymap-prefix "C-x L")
;;   (:when-loaded
;;     (:with-map lsp-mode-map
;;       (:bind "M-<return>" lsp-execute-code-action))
;;     (:hook lsp-lens-mode)
;;     (:with-mode lsp-completion-mode
;;       (:hook (lambda ()
;;                (setf (alist-get 'lsp-capf completion-category-defaults)
;;                      '((styles . (orderless flex)))))))
;;     (:with-mode python-ts-mode
;;       (:hook
;;        lsp-inlay-hints-mode
;;        (lambda ()
;;          (require 'lsp-pyright)
;;          (setq lsp-pyright-langserver-command "basedpyright")
;;          (lsp-deferred))))

;;     (:with-mode java-ts-mode
;;       (:hook
;;        (lambda ()
;;          (require 'lsp-java)
;;          (lsp-deferred))))

;;     (:with-mode swift-mode
;;       (:hook
;;        (lambda ()
;;          (require 'lsp-sourcekit)
;;          (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
;;          (lsp-deferred))))

;;     (:with-mode lsp-tailwindcss-major-modes
;;       (:hook
;;        (lambda ()
;;          (setq lsp-tailwindcss-add-on-mode t))))

;;     (:with-mode (clojure-mode clojurescript-mode clojurec-mode)
;;       (:hook
;;        (lambda ()
;;          (aggressive-indent-mode)
;;          (setq-local completion-at-point-functions
;;                      (list (cape-capf-super #'cider-complete-at-point #'lsp-completion-at-point)))
;;          (lsp-deferred))))

;;     (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

(when (featurep 'lsp-mode)
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

  (setup dap-mode
    (:load-after lsp-mode)
    (:when-loaded
      (:option dap-auto-configure-features '(sessions locals controls tooltip))
      (:with-map dap-mode-map
        (:bind
         "C-x D D" dap-debug
         "C-x D d" dap-debug-last)))
    (:with-mode python-ts-mode
      (:hook
       (lambda ()
         (require 'dap-python)
         (setq dap-python-debugger 'debugpy))))

    (:with-mode java-ts-mode
      (:hook
       (lambda ()
         (require 'dap-java))))))


(setup lsp-bridge
  (:pkg (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                    :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                    :build (:not compile)))
  (:option
   lsp-bridge-python-command "python-for-lsp-bridge"

   lsp-bridge-enable-completion-in-minibuffer t
   lsp-bridge-enable-with-tramp t
   ;; ui configuration
   lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
   lsp-bridge-signature-show-with-frame-position "top-right"
   lsp-bridge-enable-mode-line nil
   lsp-bridge-enable-hover-diagnostic t
   lsp-bridge-enable-inlay-hint t
   lsp-bridge-enable-hover-diagnostic t
   lsp-bridge-enable-org-babel t

   ;; acm configuration
   acm-candidate-match-function 'orderless-flex
   acm-backend-yas-match-by-trigger-keyword t
   acm-enable-capf t
   acm-enable-tabnine t

   ;;lsp-server configuartion
   lsp-bridge-nix-lsp-server 'nixd
   ;; (acm-enable-tempel t)
   ;; (acm-enable-codeium t)
   ;; (acm-enable-citre t)
   ;; :bind (:map acm-mode-map
   ;;         ("C-n" . acm-select-next)
   ;;         ("C-p" . acm-select-prev))
   )
  (:when-loaded
    (add-to-list 'acm-backend-capf-mode-list 'clojure-mode)
    (add-to-list 'acm-backend-capf-mode-list 'clojurescript-mode)

    (setq lsp-bridge-get-project-path-by-filepath
          (lambda (filepath)
            (or (when-let* ((project (project-current nil (file-name-directory filepath)))
                            (root (project-root project)))
                  (expand-file-name root))
                (file-name-directory filepath))))

    (yas-global-mode 1)
    (global-lsp-bridge-mode)))

(when (featurep 'lsp-mode)
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

  (setup cape
    (:load-after corfu)
    (:when-loaded
      (add-to-list 'completion-at-point-functions #'cape-emoji)
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
      (add-to-list 'completion-at-point-functions #'cape-file))))

(provide 'init-completion)
;;; init-completion.el ends here
