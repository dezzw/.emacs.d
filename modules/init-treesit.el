;;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  :straight '(:type git :host github :repo "LuciusChen/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  (treesit-auto-add-to-auto-mode-alist '(bash bibtex cmake commonlisp css dockerfile html java javascript json latex make lua org python rust ruby sql toml typescript typst vue yaml))
  (global-treesit-auto-mode))

(provide 'init-treesit)
