;;; init-ai.el --- AI/LLM related -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup gptel
  (:also-load lib-gptel)
  (:option gptel-default-mode 'org-mode
           gptel-model "openrouter/auto")
  (:when-loaded
    (setq gptel-backend (+gptel-make-netint-backend)))
  (:with-hook gptel-post-stream-hook
    (:hook (lambda () (meow-insert-exit)))
    (:hook gptel-auto-scroll))
  (:hooks gptel-post-response-hook gptel-end-of-response))

(setup agent-shell
  (:global-bind "C-c a a" 'agent-shell
                "C-c a A" 'agent-shell-new-shell
                "C-c a d" 'agent-shell-send-dwim)
  (:when-loaded
    (setopt agent-shell-show-usage-at-turn-end t
            agent-shell-prefer-viewport-interaction t
            ;;; claude code
            agent-shell-anthropic-claude-environment
            (agent-shell-make-environment-variables :inherit-env t)
            ;;; codex
            agent-shell-openai-codex-environment
            (agent-shell-make-environment-variables :inherit-env t)
            agent-shell-openai-authentication
            (agent-shell-openai-make-authentication :api-key (lambda () (getenv "OPENAI_API_KEY")))
            )
    (setq agent-shell-preferred-agent-config
          (agent-shell-cursor-make-agent-config)))
  (:after magit
    (defun +agent-shell-generate-commit ()
      "Generate conventional commit message from staged changes."
      (interactive)
      (if (magit-staged-files)
          (agent-shell-insert
           :submit t
           :text "Commit changes with conventional message")
        (user-error "No staged changes")))

    (defun +agent-shell-review-commit ()
      "Send the commit at point to agent-shell for review."
      (interactive)
      (if-let* ((commit (magit-commit-p (magit-thing-at-point 'git-revision t))))
          (agent-shell-insert
           :submit t
           :text (format "Review commit: %s" commit))
        (user-error "No magit commit at point")))

    (:with-map magit-mode-map
      (:bind "C-c C-g" '+agent-shell-generate-commit
             "C-c C-r" '+agent-shell-review-commit))))

(provide 'init-ai)
;;; init-ai.el ends here
