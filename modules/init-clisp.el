;;; -*- lexical-binding: t -*-

(use-package sly
  :straight t
  :commands (sly sly-connect)
  :init
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -Q run"))

(provide 'init-clisp)
