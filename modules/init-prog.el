;;; init-prog.el --- Programming language support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode jsp-mode web-mode "Web")
(define-derived-mode basilisp-ts-mode clojure-ts-mode "Basilisp")

(setup (:with-mode vue-mode (:match-file "*.vue"))
  (:with-mode jsp-mode (:match-file "*.jsp"))
  (:with-mode emacs-lisp-mode (:match-file "*.el"))
  (:with-mode my-html-mode (:match-file "*.html"))
  (:with-mode java-ts-mode (:match-file "*.java"))
  (:with-mode python-ts-mode (:match-file "*.py"))
  (:with-mode yaml-ts-mode
    (:match-file "*.yaml")
    (:match-file "*.yml"))
  (:with-mode lua-ts-mode (:match-file "*.lua"))
  (:with-mode tsx-ts-mode
    (:match-file "*.tsx")
    (:match-file "*.jsx"))
  (:with-mode js-ts-mode
    (:match-file "*.js")
    (:match-file "*.es6"))
  (:with-mode typescript-ts-mode
    (:match-file "*.mjs")
    (:match-file "*.mts")
    (:match-file "*.cjs")
    (:match-file "*.ts"))
  (:with-mode json-ts-mode (:match-file "*.json"))
  (:with-mode nix-ts-mode (:match-file "*.nix"))
  (:with-mode dockerfile-ts-mode (:match-file "*.Dockerfile"))
  (:with-mode rust-ts-mode (:match-file "*.rs"))
  (:with-mode go-ts-mode (:match-file "*.go"))
  (:with-mode markdown-ts-mode (:match-file "*.md"))
  (:with-mode basilisp-ts-mode (:match-file "*.lpy")))

(setup display-fill-column-indicator (:hook-into prog-mode))
(setup display-line-numbers (:hook-into prog-mode))

(setup python
  (:option python-indent-guess-indent-offset t
           python-indent-guess-indent-offset-verbose nil))

(setup go
  (:option tab-width 4
           go-ts-mode-indent-offset 4))

;; or the product can be set from a comment on the first line
;; -- -*- mode: sql; sql-product: mysql; -*-
;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
(setup sql
  (:when-loaded
    (sql-set-product 'mysql)))

(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-enable-current-column-highlight t))

(setup js-ts-mode
  (:also-load lib-js)
  (:when-loaded
    (setq-default js-indent-level 2)
    (:option treesit-simple-imenu-sort nil)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js-ts-mode))
    (+major-mode-lighter 'js-ts-mode "JS")))

(setup verb
  (:option verb-babel-timeout 30.0))

(setup lisp-mode
  (:also-load lib-lisp)
  (:autoload macrostep-expand)
  (:global-bind "<remap> <eval-expression>" 'pp-eval-expression)
  (:with-map emacs-lisp-mode-map
    (:bind
     "C-x C-e" +eval-last-sexp-or-region
     "C-c C-e" pp-eval-expression
     "C-c C-l" +load-this-file
     "C-c x" macrostep-expand))
  (:advice pp-display-expression :after +make-read-only)
  (:hooks emacs-lisp-mode-hook +maybe-set-bundled-elisp-readonly))

(defun +xref-quit-window ()
  "Quit the xref window."
  (let ((xref-window (get-buffer-window "*xref*")))
    (when xref-window
      (quit-window nil xref-window))))

(setup project
  (:when-loaded
    (:global-bind "C-c p" (identity project-prefix-map))
    (:with-map project-prefix-map
      (:bind "t" project-vterm))
    (:set project-vc-extra-root-markers
          '("package.json" "deps.edn" "project.clj" "Package.swift" ".envrc" ".tags" ".project"))))

(setup xref
  (:option xref-auto-jump-to-first-xref 'move)
  (:hooks xref-after-jump-hook +xref-quit-window))

(setup treesit
  (:also-load lib-treesit)
  (:when-loaded
    (:option treesit-language-source-alist +treesit-language-source-alist)))

(setup tramp
  (:option tramp-default-method "ssh"
           tramp-auto-save-directory (expand-file-name "tramp-autosaves/" user-emacs-directory)
           enable-remote-dir-locals t
           remote-file-name-inhibit-cache 60))

(setup compile
  (:option compilation-always-kill t
           compilation-ask-about-save nil
           compilation-scroll-output 'first-error)
  (:when-loaded
    (autoload 'comint-truncate-buffer "comint" nil t)
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
    (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)))

(setup envrc
  (:hooks after-init-hook envrc-global-mode)
  (:bind "C-c e" envrc-command-map))

;; Apheleia does not support format-in-region.
(setup apheleia
  (:global-bind "C-c C-x C-f" 'apheleia-format-buffer)
  (:hook-into prog-mode)
  (:when-loaded
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
    (setf (alist-get 'my-html-mode apheleia-mode-alist) 'prettier-html)
    (setf (alist-get 'sql-mode apheleia-mode-alist) 'pgformatter)
    (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier)))

(setup reformatter
  (:require reformatter)
  (:with-mode python-ts-mode
    (reformatter-define ruff-format
      :program "ruff"
      :args '("format" "-"))))

(setup mmm-mode
  (:with-mode nxml-mode
    (:hook mmm-mode))
  (:when-loaded
    (:option mmm-parse-when-idle t
             mmm-global-classes nil
             mmm-classes-alist nil
             mmm-mode-ext-classes-alist nil
             mmm-submode-decoration-level 0)
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

(setup indent-bars
  (:with-mode (java-ts-mode python-ts-mode vue-mode typescript-ts-mode js-ts-mode)
    (:hook indent-bars-mode))
  (:when-loaded
    (:option indent-bars-color '(highlight :face-bg t :blend 0.15)
             indent-bars-pattern "."
             indent-bars-width-frac 0.1
             indent-bars-pad-frac 0.1
             indent-bars-zigzag nil
             indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
             indent-bars-highlight-current-depth '(:blend 0.5 :width 0.5)
             indent-bars-display-on-blank-lines t
             indent-bars-treesit-support t
             indent-bars-no-descend-string t
             indent-bars-prefer-character t
             indent-bars-no-stipple-char ?\u2502
             indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                                 if_statement with_statement while_statement)))))

(setup separedit
  (:with-map prog-mode-map (:bind "C-c '" separedit))
  (:with-map minibuffer-mode-map (:bind "C-c '" separedit))
  (:with-map help-mode-map (:bind "C-c '" separedit))
  (:option separedit-default-mode 'org-mode))

(setup flymake
  ;; When `flymake-no-changes-timeout' is nil, Eglot must handle
  ;; diagnostics refresh explicitly.
  (:set flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe)
  (when (version<= "31" emacs-version)
    (:set flymake-show-diagnostics-at-end-of-line t)))

(setup eldoc-box
  (:hooks eglot-managed-mode-hook eldoc-box-hover-mode))

(setup eglot
  (:with-mode (python-ts-mode js-ts-mode tsx-ts-mode vue-mode latex-mode)
    (:hook eglot-ensure))
  (:when-loaded
    (:also-load lib-eglot)
    (:set eglot-code-action-indications '(eldoc-hint)
          eglot-max-file-watches 30000
          eglot-events-buffer-config '(:size 0 :format full))
    ;; Ignore server formatting, format with Apheleia instead.
    (:set eglot-ignored-server-capabilities '(:documentFormattingProvider
                                              :documentRangeFormattingProvider))
    (add-to-list 'eglot-server-programs '(python-ts-mode . ("rass" "python")))
    (add-to-list 'eglot-server-programs `((vue-mode vue-ts-mode typescript-ts-mode) . ("rass" "vuetail")))
    (add-to-list 'eglot-server-programs '(my-html-mode . ("vscode-html-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (:with-hook eglot-managed-mode-hook
      (:hook (lambda ()
               (setq eldoc-documentation-functions
                     (cons #'flymake-eldoc-function
                           (remove #'flymake-eldoc-function eldoc-documentation-functions)))
               (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))))

(setup eglot-booster
  (:option eglot-booster-io-only t)
  (:with-hook after-init-hook
    (:hook eglot-booster-mode)))

(setup eglot-x
  (:hooks eglot-managed-mode-hook eglot-x-setup))

(setup citre
  (:global-bind "C-c c j" 'citre-jump
                "C-c c J" 'citre-jump-back
                "C-c c p" 'citre-ace-peek
                "C-c c u" 'citre-update-this-tags-file)
  (:when-loaded
    (:also-load citre-config)
    (:set citre-auto-enable-citre-mode-backends '(eglot tags global)
          citre-completion-backends '(eglot tags global))))

(setup topsy
  (:hook-into prog-mode))

(provide 'init-prog)
;;; init-prog.el ends here
