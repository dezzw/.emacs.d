;;; init-prog.el --- prog mode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package elec-pair
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode minibuffer-mode) . electric-pair-mode))

(use-package electric
  :straight nil
  :config (electric-indent-mode))

;; (use-package rainbow-mode
;;   :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode))

(use-package colorful-mode
  :straight t
  :hook (prog-mode text-mode)
  :custom
  (colorful-use-prefix t))


;; (use-package prism
;;   :straight t)

(use-package aggressive-indent
  :straight t
  :hook (python-mode emacs-lisp-mode clojure-mode clojurescript-mode fennel-mode))

(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.2))

(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2))

(use-package savefold
  ;; :disable
  :straight '(:host github :repo "jcfk/savefold.el")
  :init
  (setq savefold-backends '(outline org hideshow))
  (setq savefold-directory (locate-user-emacs-file "savefold"))  ;; default

  :config
  (savefold-mode 1))

(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
						(line-beginning-position 2)))))

(use-package vdiff
  :straight t
  :commands (vdiff-buffer))

(use-package restclient
  :straight t
  :mode (("\\.rest\\'" . restclient-mode)))

;;; try out both developing documents
(use-package devdocs
  :straight t
  :commands (devdocs-lookup))

(use-package compile
  :defer t
  :hook ((compilation-filter . ansi-color-compilation-filter))
  :bind (("C-x C-m" . recompile))
  :config
  (setopt compilation-scroll-output t)
  (setopt compilation-ask-about-save nil)
  (require 'ansi-color))

(use-package smart-compile
  :straight t
  :after compile)

(use-package compile-multi
  :straight t
  :after compile)

(use-package consult-compile-multi
  :when (featurep 'compile-multi)
  :straight t
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-nerd-icons
  :when (featurep 'compile-multi)
  :straight t
  :after nerd-icons-completion
  :after compile-multi
  :demand t)

(use-package compile-multi-embark
  :when (featurep 'compile-multi)
  :straight t
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))

(use-package tabnine
  :straight t
  ;; :diminish "⌬"
  :defer t
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 0)
  :hook (kill-emacs . tabnine-kill-process))
;; :config
;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;; ;; (add-to-list 'nerd-icons-corfu-mapping `(tabnine ,(nerd-icons-codicon "nf-cod-hubot") :face font-lock-warning-face) t)
;; (tabnine-start-process)
;; :bind
;; (:map  tabnine-completion-map
;; 	 ("<tab>" . tabnine-accept-completion)
;; 	 ("TAB" . tabnine-accept-completion)
;; 	 ("M-f" . tabnine-accept-completion-by-word)
;; 	 ("M-<return>" . tabnine-accept-completion-by-line)
;; 	 ("C-g" . tabnine-clear-overlay)
;; 	 ("M-[" . tabnine-previous-completion)
;; 	 ("M-]" . tabnine-next-completion)))

(use-package jinx
  :straight '(jinx :host github :repo "minad/jinx" :fork "dezzw")
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  ;; See issue https://github.com/minad/jinx/issues/4
  ;; This is the syntax table approach. It changes CJK characters from "w" (
  ;; word constituent) to "_" (symbol constituent). You can use `describe-char'
  ;; to view a characters' specific syntax category (from major mode syntax table).
  ;; Emacs 29 supports Unicode 15, the code charts of which can be found at
  ;; http://www.unicode.org/charts/ (use mouse hover to show the specific range)
  (let ((st jinx--base-syntax-table))
    (modify-syntax-entry '(#x4E00 . #x9FFF) "_" st)   ; CJK Unified Ideographs
    (modify-syntax-entry '(#x3400 . #x4DBF) "_" st)   ; CJK Unified Ideographs Extension A
    (modify-syntax-entry '(#x20000 . #x2A6DF) "_" st) ; CJK Unified Ideographs Extension B
    (modify-syntax-entry '(#x2A700 . #x2B73F) "_" st) ; CJK Unified Ideographs Extension C
    (modify-syntax-entry '(#x2B740 . #x2B81F) "_" st) ; CJK Unified Ideographs Extension D
    (modify-syntax-entry '(#x2B820 . #x2CEAF) "_" st) ; CJK Unified Ideographs Extension E
    (modify-syntax-entry '(#x2CEB0 . #x2EBEF) "_" st) ; CJK Unified Ideographs Extension F
    (modify-syntax-entry '(#x30000 . #x3134F) "_" st) ; CJK Unified Ideographs Extension G
    (modify-syntax-entry '(#x31350 . #x323AF) "_" st) ; CJK Unified Ideographs Extension H
    (modify-syntax-entry '(#x2EBF0 . #x2EE5F) "_" st) ; CJK Unified Ideographs Extension I
    ))

(use-package eee
  :straight '(:type git :host github :repo "eval-exec/eee.el"
                    :files (:defaults "*.el" "*.sh"))
  :config
  (setq ee-terminal-command "wezterm"))

(use-package editorconfig
  :demand t
  :config
  (defun oxcl/update-indent-bars-with-editorconfig (size)
    (when (bound-and-true-p indent-bars-mode)
      (setq indent-bars-spacing-override size)
      (indent-bars-reset)))
  (dolist (_mode editorconfig-indentation-alist)
    (let ((_varlist (cdr _mode)))
      (setcdr _mode (append '((_ . oxcl/update-indent-bars-with-editorconfig))
                            (if (listp _varlist) _varlist `(,_varlist))))))
  (editorconfig-mode 1))


(provide 'init-prog)
;;; init-prog.el ends here
