;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Lots of stuff from http://doc.norang.ca/org-mode.html
(setup org
  (:global-bind "C-c L" 'org-store-link
                "C-c C-o" 'org-open-at-point
                "C-M-<up>" 'org-up-element)
  ;; 一般这个函数都是在 org 启动后调用，如果 org 没有启动则会报错。
  ;; Wrong type argument: commandp, dired-copy-images-links
  (:global-bind "C-c n m" 'dired-copy-images-links)
  (:when-loaded
    (:also-load lib-org)
    (:also-load lib-org-archive-hierachical)
    (:option
     org-directory *org-path*
     ;; emphasis
     org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:]"
                                      "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                                      "[:space:]"
                                      "."
                                      1)
     org-match-substring-regexp (concat
                                 "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
                                 "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
                                 "\\|"
                                 "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
                                 "\\|"
                                 "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")
     org-image-actual-width nil
     ;; remove org-src content indent
     org-edit-src-content-indentation 0
     org-src-preserve-indentation nil
     ;; https://git.tecosaur.net/tec/org-mode.git version only
     org-fontify-semantic-seperator nil
     org-goto-interface 'outline-path-completion
     ;; Various preferences
     org-log-done t
     org-startup-indented t
     org-edit-timestamp-down-means-later t
     org-hide-emphasis-markers t
     org-fold-catch-invisible-edits 'show
     org-export-coding-system 'utf-8
     org-fast-tag-selection-single-key 'expert
     org-html-validation-link nil
     org-export-kill-product-buffer-when-displayed t
     org-tags-column 80
     ;; refiling
     org-refile-use-cache nil
     ;; Keep refiling local by default after removing agenda-specific workflow.
     org-refile-targets '((nil :maxlevel . 5))
     ;; Allow refile to create parent tasks with confirmation
     org-refile-allow-creating-parent-nodes 'confirm
     ;; Targets start with the file name - allows creating level 1 tasks
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil
     ;; archive
     org-archive-mark-done nil
     org-archive-location "%s_archive::* Archive"
     org-archive-default-command 'org-archive-subtree-hierarchical
     ;; TODO
     ;; HOLD(h@)       ; 进入时添加笔记
     ;; HOLD(h/!)      ; 离开时添加变更信息
     ;; HOLD(h@/!)     ; 进入时添加笔记，离开时添加变更信息
     org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
       (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c/!)")
       (sequence "WAITING(w/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c/!)"))
     org-todo-repeat-to-state "NEXT"
     org-todo-keyword-faces
     '(("NEXT" :inherit warning)
       ("PROJECT" :inherit font-lock-string-face))
     ;; Exclude DONE state tasks from refile targets
     org-refile-target-verify-function (lambda ()
                                         (not (member
                                               (nth 2 (org-heading-components))
                                               org-done-keywords))))
    (:with-mode org-mode
      (:hook (lambda () (electric-pair-local-mode -1)))
      (:hook (lambda () (setq truncate-lines nil))))
    (+org-emphasize-bindings)
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
    (org-element-update-syntax)))

(setup image-slicing
  (:load-after org))

(setup org-download
  (:load-after org)
  (:when-loaded
    (:option org-download-image-dir "./images/")))

(setup ob-core
  (:load-after org)
  (:when-loaded
    (:also-load
     ob-async
     ob-python
     ob-latex
     ob-verb)
    (:option
     ;; 这里应该就是 .zshrc 里面配置的 python3
     org-babel-python-command "python3")
    (org-babel-do-load-languages
     'org-babel-load-languages '((python . t)
                                 (shell . t)
                                 (verb . t)
                                 (latex . t)))))

(setup denote
  (:defer (:require denote))
  (:when-loaded
    (:global-bind "C-c n n" 'denote-open-or-create
                  "C-c n d" 'denote-sort-dired
                  "C-c n l" 'denote-link
                  "C-c n L" 'denote-add-links
                  "C-c n b" 'denote-backlinks
                  "C-c n r" 'denote-rename-file
                  "C-c n R" 'denote-rename-file-using-front-matter)
    (:option denote-directory (expand-file-name "denote" *org-path*)
             denote-save-buffers nil
             denote-known-keywords '("emacs" "private")
             denote-infer-keywords t
             denote-sort-keywords t
             denote-prompts '(title file-type keywords)
             denote-excluded-directories-regexp nil
             denote-excluded-keywords-regexp nil
             denote-rename-confirmations '(rewrite-front-matter modify-file-name)
             denote-date-prompt-use-org-read-date t
             denote-backlinks-show-context t
             denote-org-front-matter
             "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+signature:  %s
#+startup: indent
\n")
    (denote-rename-buffer-mode 1)))

(setup ox-latex
  (:load-after org)
  (:when-loaded
    (:option
     org-latex-pdf-process '("latexmk -pdflatex='xelatex -shell-escape -interaction=nonstopmode' -pdf -f %f")
     org-preview-latex-default-process 'dvisvgm
     org-format-latex-options '(:foreground default :background "Transparent" :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                                            ("begin" "$1" "$" "$$" "\\(" "\\["))
     org-latex-listings 'minted
     org-latex-minted-options '(("breaklines" "true")
                                ("fontsize" "\\small")
                                ("bgcolor" "none"))
     ;; org-latex-compiler "xelatex"
     org-latex-packages-alist
     '(;; hook right arrow with text above and below
       ;; https://tex.stackexchange.com/questions/186896/xhookrightarrow-and-xmapsto
       ("" "svg" t)
       ("" "svg-extract" t)

       ("" "mathtools" t)
       ("" "amsmath" t)
       ("" "amssymb" t)
       ;; for mapsfrom
       ;; see: https://tex.stackexchange.com/questions/26508/left-version-of-mapsto
       ("" "stmaryrd" t)
       ("" "mathrsfs" t)
       ("" "tikz" t)
       ("" "tikz-cd" t)
       ;; ("" "quiver" t)
       ;; see https://castel.dev/post/lecture-notes-2/
       ("" "import" t)
       ("" "xifthen" t)
       ("" "pdfpages" t)
       ("" "transparent" t)
       ;; algorithm
       ;; https://tex.stackexchange.com/questions/229355/algorithm-algorithmic-algorithmicx-algorithm2e-algpseudocode-confused
       ("ruled,linesnumbered" "algorithm2e" t)
       ("" "minted" t)
       ("" "xcolor" t)
       ;; You should not load the algorithm2e, algcompatible, algorithmic packages if you have already loaded algpseudocode.
       ;; ("" "algpseudocode" t)
       ;; for chinese preview
     ;; ("fontset=LXGW WenKai,UTF8" "ctex" t)
       ))))

(setup visual-fill-column
  (:load-after org)
  (:with-mode org-mode
    (:hook (lambda ()
             (setq visual-fill-column-width 110
                   visual-fill-column-center-text t)
             (visual-fill-column-mode 1)))))

(setup valign
  (:load-after org)
  (:with-mode org-mode (:hook valign-mode)))

(setup org-appear
  (:load-after org)
  (:with-mode org-mode (:hook org-appear-mode)))

(setup org-tidy
  (:load-after org)
  (:with-mode org-mode
    (:hook (lambda ()
             (setq org-tidy-properties-style 'fringe)
             (org-tidy-mode 1)))))

(setup org-modern
  (:load-after org)
  (:when-loaded
    (:with-mode org-mode
      (:hook org-modern-mode)
      (:hook (lambda ()
               "Beautify Org Checkbox Symbol"
               (push '("[ ]" . "☐") prettify-symbols-alist)
               (push '("[X]" . "☑" ) prettify-symbols-alist)
               (push '("[-]" . #("□–" 0 2 (composition ((2))))) prettify-symbols-alist)
               (prettify-symbols-mode))))
    (:option org-modern-star 'replace
             org-modern-replace-stars "❑❍❑❍❑❍"
             org-hide-emphasis-markers t
             org-tags-column 0
             org-modern-block-fringe 2
             org-catch-invisible-edits 'show-and-error
             org-special-ctrl-a/e t
             org-insert-heading-respect-content t
             org-modern-table-vertical 0
             org-modern-table-horizontal 0.2
             org-modern-checkbox nil
             org-ellipsis "[+]")

    (:also-load
     org-modern-indent)
    (add-hook 'org-mode-hook #'org-modern-indent-mode 90)

    ;; 美化 checkbox，unchecked 和 checked 分别继承 TODO 的 TODO 和 DONE 的颜色。
    ;; https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes
    (defface org-checkbox-todo-text
      '((t (:foreground unspecified :inherit org-todo)))
      "Face for the text part of an unchecked org-mode checkbox.")

    (defface org-checkbox-done-text
      '((t (:foreground unspecified :inherit org-done :strike-through t)))
      "Face for the text part of a checked org-mode checkbox.")

    (defface org-checkbox-partial-text
      '((t (:foreground unspecified :inherit org-todo)))
      "Face for the text part of a partially checked org-mode checkbox.")

    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[ \\][^\n]*\n\\)"
        1 'org-checkbox-todo-text prepend)
       ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[X\\][^\n]*\n\\)"
        1 'org-checkbox-done-text prepend)
       ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[-\\][^\n]*\n\\)"
        1 'org-checkbox-partial-text prepend))
     'append)))

(provide 'init-org)
;;; init-org.el ends here
