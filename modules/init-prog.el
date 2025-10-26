;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode jsp-mode web-mode "Web")
(define-derived-mode wxss-mode css-mode "CSS")
(define-derived-mode wxml-mode html-mode "HTML")
(define-derived-mode basilisp-ts-mode clojure-ts-mode "Basilisp")

(setup (:with-mode vue-mode (:match-file "*.vue"))
  (:with-mode jsp-mode (:match-file "*.jsp"))
  (:with-mode emacs-lisp-mode (:match-file "*.el"))
  (:with-mode wxss-mode (:match-file "*.wxss"))
  (:with-mode my-html-mode (:match-file "*.wxml")
              (:match-file "*.html"))
  (:with-mode java-ts-mode (:match-file "*.java"))
  (:with-mode python-ts-mode (:match-file "*.py"))
  (:with-mode yaml-ts-mode (:match-file "*.yaml") (:match-file "*.yml"))
  (:with-mode lua-ts-mode (:match-file "*.lua"))
  (:with-mode tsx-ts-mode (:match-file "*.tsx")
              (:match-file "*.jsx"))
  (:with-mode js-mode (:match-file "*.js")
              (:match-file "*.es6"))
  (:with-mode typescript-ts-mode (:match-file "*.mjs")
              (:match-file "*.mts")
              (:match-file "*.cjs")
              (:match-file "*.ts"))
  (:with-mode json-ts-mode (:match-file "*.json"))
  (:with-mode nix-ts-mode (:match-file "*.nix"))
  (:with-mode lua-ts-mode (:match-file "*.lua"))
  (:with-mode fennel-mode (:match-file "*.fnl"))
  (:with-mode dockerfile-ts-mode (:match-file "*.Dockerfile"))
  (:with-mode rust-ts-mode (:match-file "*.rs"))
  (:with-mode go-ts-mode (:match-file "*.go"))
  (:with-mode markdown-ts-mode (:match-file "*.md"))
  (:with-mode basilisp-ts-mode (:match-file "*.lpy")))

(setup display-fill-column-indicator (:hook-into prog-mode))
(setup display-line-numbers (:hook-into prog-mode))

(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-enable-current-column-highlight t))

(setup verb (:option verb-babel-timeout 60.0))

(setup python
  (:option python-indent-guess-indent-offset t
           python-indent-guess-indent-offset-verbose nil))
(setup go
  (:option tab-width 4
           go-ts-mode-indent-offset 4))

(setup json-ts-mode
  (:option json-ts-mode-indent-offset 4))

;; Not support format in region
(setup apheleia
  (:when-loaded
    (:global "C-c C-x C-f" apheleia-format-buffer)
    ;; $ brew install isort black google-java-format stylua libxml2
    ;; $ npm install -g prettier
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format" "--aosp" filepath))
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "--indent-type" "Spaces" filepath))
    (setf (alist-get 'xmllint apheleia-formatters)
          '("xmllint" "--encode" "utf-8" "--format" "-"))

    (setf (alist-get 'python-ts-mode     apheleia-mode-alist) '(isort black))
    (setf (alist-get 'my-html-mode       apheleia-mode-alist) 'prettier-html)
    (setf (alist-get 'sql-mode           apheleia-mode-alist) 'pgformatter)
    (setf (alist-get 'xml-mode           apheleia-mode-alist) 'xmllint)
    (setf (alist-get 'nxml-mode          apheleia-mode-alist) 'xmllint)
    (setf (alist-get 'css-mode           apheleia-mode-alist) 'prettier)
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier)))

(setup reformtter
  (:pkg reformatter)
  (:with-mode python-ts-mode
    (reformatter-define black-format
                        :program "black"
                        :args '("-"))))

(setup mmm-mode
  (:with-mode prog-mode (:require mmm-mode))
  (:when-loaded
    (:option mmm-parse-when-idle t
             mmm-global-classes nil
             mmm-classes-alist nil
             mmm-mode-ext-classes-alist nil
             mmm-submode-decoration-level 0)
    (:hook-into nxml-mode)
    (mmm-add-classes
     '((nxml-sql-select :submode sql-mode
                        :front "<select[^>]*>[ \t]*\n" :back "[ \t]*</select>")
       (nxml-sql-insert :submode sql-mode
                        :front "<insert[^>]*>[ \t]*\n" :back "[ \t]*</insert>")
       (nxml-sql-update :submode sql-mode
                        :front "<update[^>]*>[ \t]*\n" :back "[ \t]*</update>")
       (nxml-sql-delete :submode sql-mode
                        :front "<delete[^>]*>[ \t]*\n" :back "[ \t]*</delete>")))
    (dolist (class '(nxml-sql-select nxml-sql-insert nxml-sql-update nxml-sql-delete))
      (mmm-add-mode-ext-class 'nxml-mode nil class))))

(setup lisp-mode
  (:also-load lib-lisp)
  (:require macrostep)
  (global-set-key [remap eval-expression] 'pp-eval-expression)
  (:with-map emacs-lisp-mode-map
    (:bind
     "C-x C-e" +eval-last-sexp-or-region
     "C-c C-e" pp-eval-expression
     "C-c C-l" +load-this-file
     "C-c x"   macrostep-expand))
  (:advice pp-display-expression :after +make-read-only)
  (:hooks emacs-lisp-mode-hook +maybe-set-bundled-elisp-readonly))

;; or the product can be set from a comment on the first line
;; -- -*- mode: sql; sql-product: mysql; -*-
;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
(setup sql (:when-loaded (sql-set-product 'mysql)))

(setup project
  (:option project-vc-extra-root-markers
           '("package.json" "deps.edn" "project.clj" "Package.swift" ".envrc" ".tags" ".project")))

(setup js
  (:also-load lib-js)
  (:when-loaded
    (setq-default js-indent-level 2)
    (+major-mode-lighter 'js-mode "JS")
    (+major-mode-lighter 'js-jsx-mode "JSX")))

;; js2-mode
(setup js2-mode
  (:when-loaded
    (:hooks js-mode-hook +enable-js2-checks-if-flymake-inactive
            js2-mode-hook +enable-js2-checks-if-flymake-inactive)
    ;; Change some defaults: customize them to override
    (setq-default js2-bounce-indent-p nil)
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    (js2-imenu-extras-setup)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (+major-mode-lighter 'js2-mode "JS2")
    (+major-mode-lighter 'js2-jsx-mode "JSX2")))

(setup xref
  ;; 用 Popper 替代了 +xref-show-xrefs 以及 :option 配置
  ;;
  ;;   (defun +xref-show-xrefs (fetcher display-action)
  ;;     "Display some Xref values produced by FETCHER using DISPLAY-ACTION.
  ;; Do not jump to the first xref, just move the focus to the xref window."
  ;;     (let ((buf (xref--show-xref-buffer fetcher
  ;;                                        `((window . ,(selected-window))
  ;;                                          (display-action . ,display-action)
  ;;                                          (auto-jump . nil)))))
  ;;       (let ((window (get-buffer-window buf)))
  ;;         (when window
  ;;           (select-window window)))))

  (defun +xref-quit-window ()
    "Quit the xref window."
    (let ((xref-window (get-buffer-window "*xref*")))
      (when xref-window
        (quit-window nil xref-window))))

  (:option xref-auto-jump-to-first-xref 'move)
  ;; (setq xref-show-xrefs-function #'+xref-show-xrefs)
  (:hooks xref-after-jump-hook +xref-quit-window))

(setup treesit
  (:also-load lib-treesit)
  (:when-loaded
    (:option treesit-language-source-alist +treesit-language-source-alist)))


(setup indent-bars
  (:with-mode (java-ts-mode python-ts-mode vue-mode typescript-mode typescript-ts-mode js-mode)
    (:require indent-bars)
    (:hook indent-bars-mode))
  (:when-loaded
    (:option indent-bars-color '(highlight :face-bg t :blend 0.15)
             indent-bars-pattern "."
             indent-bars-width-frac 0.1
             indent-bars-pad-frac 0.1
             indent-bars-zigzag nil
             indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
             indent-bars-highlight-current-depth '(:blend 0.5 :width 0.5) ; pump up the BG blend on current
             indent-bars-display-on-blank-lines t
             ;; indent-bars-display-on-blank-lines nil
             indent-bars-treesit-support t
             indent-bars-no-descend-string t
             indent-bars-prefer-character t
             indent-bars-no-stipple-char ?\u2502
             indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                                 if_statement with_statement while_statement)))))

(setup separedit
  (:defer (:require separedit))
  (:when-loaded
    (:with-map prog-mode-map (:bind "C-c '" separedit))
    (:with-map minibuffer-mode-map (:bind "C-c '" separedit))
    (:with-map help-mode-map (:bind "C-c '" separedit))
    (:option separedit-default-mode 'org-mode)))

(setup envrc
  (:hooks after-init-hook envrc-global-mode)
  (:bind "C-c e" envrc-command-map))

(setup tramp
  (:option tramp-default-method "ssh"
           tramp-auto-save-directory (expand-file-name "tramp-autosaves/" user-emacs-directory)
           enable-remote-dir-locals t
           remote-file-name-inhibit-cache 60))

(setup flymake
  (:defer (:require flymake))
  (:when-loaded
    ;; 注意：当 `flymake-no-changes-timeout` 被设置为 nil 时，
    ;; 需要实现 `eglot-handle-notification` 的 `:after` 方法。
    (setopt flymake-no-changes-timeout nil
            flymake-fringe-indicator-position 'right-fringe)
    (when (version<= "31" emacs-version)
      (setopt flymake-show-diagnostics-at-end-of-line t))
    (:with-mode prog-mode (:hook flymake-mode))
    (:with-mode emacs-lisp-mode (:hook (lambda()(flymake-mode -1))))))

(setup eglot
  (:when-loaded
    (:also-load lib-eglot)
    (:with-mode (python-ts-mode js-ts-mode typescript-mode tsx-ts-mode vue-mode latex-mode)
      (:hook eglot-ensure))
    (setopt eglot-code-action-indications '(eldoc-hint)
            eglot-events-buffer-config '(:size 0 :format full) ;; 取消 eglot log
            ;; ignore lsp formatting provider, format with apheleia.
            eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                :documentRangeFormattingProvider))
    (add-to-list 'eglot-server-programs '(my-html-mode . ("vscode-html-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs `((vue-mode vue-ts-mode typescript-ts-mode typescript-mode) . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
    (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (setq-default
       eglot-workspace-configuration
       '(:basedpyright.analysis (
           :typeCheckingMode "off"
           :diagnosticSeverityOverrides (
             :reportUnusedCallResult "none"
           )
           :inlayHints (
             :callArgumentNames :json-false
           )
         )))
    ;; https://github.com/joaotavora/eglot/discussions/898
    (:with-hook eglot-managed-mode-hook
      (:hook (lambda ()
               ;; Show flymake diagnostics first.
               (setq eldoc-documentation-functions
                     (cons #'flymake-eldoc-function
                           (remove #'flymake-eldoc-function eldoc-documentation-functions)))
               ;; Show all eldoc feedback.
               (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))))

(setup eglot-booster
  (:pkg (eglot-booster :host github :repo "jdtsmith/eglot-booster"))
  (:load-after eglot)
  (:option eglot-booster-io-only t)
  (:when-loaded (eglot-booster-mode)))

(setup eglot-x
  (:pkg (eglot-x :host github :repo "nemethf/eglot-x"))
  (:hooks eglot-managed-mode-hook eglot-x-setup))

;; (setup lsp-proxy
;;   (:pkg (lsp-proxy :host github :repo "jadestrong/lsp-proxy" :files ("*.el"))))

(setup compile
  (:option compilation-always-kill t       ; kill compilation process before starting another
           compilation-ask-about-save nil  ; save all buffers on `compile'
           compilation-scroll-output 'first-error)
  (:when-loaded
    (autoload 'comint-truncate-buffer "comint" nil t)
    (add-hook 'compilation-filter-hook #'comint-truncate-buffer)))


(setup citre
  (:defer (:require citre))
  (:also-load citre-config))

(provide 'init-prog)
;;; init-prog.el ends here
