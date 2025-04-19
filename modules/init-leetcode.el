;;; init-leetcode.el --- leetcode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package leetcode
  :straight '(:type git :host github :repo "ginqi7/leetcode-emacs")
  :commands (leetcode-list-all)
  :config
  (setq leetcode-language "swift"))

(provide 'init-leetcode)
;;; init-leetcode.el ends here
