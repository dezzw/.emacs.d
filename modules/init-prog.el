;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode jsp-mode web-mode "Web")
(define-derived-mode wxss-mode css-mode "CSS")
(define-derived-mode wxml-mode html-mode "HTML")
(define-derived-mode basilisp-mode clojure-mode "Basilisp")

(setup (:with-mode vue-mode (:file-match "\\.vue\\'"))
  (:with-mode jsp-mode (:file-match "\\.jsp\\'"))
  (:with-mode emacs-lisp-mode (:file-match "\\.el\\'"))
  (:with-mode wxss-mode (:file-match "\\.wxss\\'"))
  (:with-mode my-html-mode (:file-match "\\.wxml\\'")
              (:file-match "\\.html\\'"))
  (:with-mode java-ts-mode (:file-match "\\.java\\'"))
  (:with-mode python-ts-mode (:file-match "\\.py\\'"))
  (:with-mode yaml-ts-mode (:file-match "\\.ya?ml\\'"))
  (:with-mode lua-ts-mode (:file-match "\\.lua\\'"))
  (:with-mode tsx-ts-mode (:file-match "\\.tsx\\'")
              (:file-match "\\.jsx\\'"))
  (:with-mode js-mode (:file-match "\\.js\\'")
              (:file-match "\\.es6\\'")
              (:file-match "\\.js\\.erb\\'")
              (:file-match "\\.es6\\.erb\\'"))
  (:with-mode typescript-ts-mode (:file-match "\\.mjs\\'")
              (:file-match "\\.mts\\'")
              (:file-match "\\.cjs\\'")
              (:file-match "\\.ts\\'"))
  (:with-mode json-ts-mode (:file-match "\\.json\\'"))
  (:with-mode nix-ts-mode (:file-match "\\.nix\\'"))
  (:with-mode lua-ts-mode (:file-match "\\.lua\\'"))
  (:with-mode fennel-mode (:file-match "\\.fnl\\'"))
  (:with-mode dockerfile-ts-mode (:file-match "\\.Dockerfile\\'"))
  (:with-mode markdown-ts-mode (:file-match "\\.md\\'"))
  (:with-mode basilisp-mode (:file-match "\\.lpy\\'")))

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

(setup apheleia
  (:with-mode prog-mode (:require apheleia)
              (:hook apheleia-global-mode))
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

(setup treesit-auto
  (:defer (:require treesit-auto))
  (:when-loaded
    (:option treesit-auto-install 'prompt)
    (treesit-auto-add-to-auto-mode-alist '(bash bibtex cmake commonlisp css dockerfile html java javascript json latex make lua org python rust ruby sql toml typescript typst vue yaml))
    (global-treesit-auto-mode)))

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
       (require 'dap-java)))))

(setup separedit
  (:defer (:require separedit))
  (:when-loaded
    (:with-map prog-mode-map (:bind "C-c '" separedit))
    (:with-map minibuffer-mode-map (:bind "C-c '" separedit))
    (:with-map help-mode-map (:bind "C-c '" separedit))
    (:option separedit-default-mode 'org-mode)))

(setup cider
  (:load-after clojure-mode))

(setup envrc
  (:hooks after-init-hook envrc-global-mode)
  (:bind "C-c e" envrc-command-map))

(provide 'init-prog)
;;; init-prog.el ends here
