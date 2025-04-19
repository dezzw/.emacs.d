;;; init-env.el --- system environment related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "TERM"))
  (exec-path-from-shell-initialize))

;; (defun environment-update ()
;;   (interactive)
;;   (let* ((shell (or (getenv "SHELL") "/bin/sh"))
;;          (command (format "%s -l -c 'env'" shell)))
;;     (with-temp-buffer
;;       (call-process-shell-command command nil t)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
;;         (let ((key (match-string 1))
;;               (val (match-string 2)))
;;           (when (string-equal key "PATH")
;;             (setenv key val)
;;             (setq exec-path (split-string val path-separator))))))))

;; (add-hook 'after-init-hook #'environment-update)

(use-package envrc
  :straight t
  :init
  (envrc-global-mode)
  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))

(provide 'init-env)
;;; init-env.el ends here
