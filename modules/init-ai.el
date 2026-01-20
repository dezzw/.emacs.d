;;; init-ai.el --- AI/LLM related -*- lexical-binding: t -*-
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

(setup gptel-agent
  (:when-loaded
    (gptel-agent-update)))

(setup agent-shell
  ;; Make agent-shell bookmark-able
  (defun my/agent-shell-bookmark (_bookmark)
    (agent-shell))
  (add-hook 'agent-shell-mode-hook
            (lambda ()
              (setq-local bookmark-make-record-function
                          (lambda ()
                            `((handler . my/agent-shell-bookmark))))))
  (:when-loaded
    (setopt agent-shell-anthropic-claude-environment
            (agent-shell-make-environment-variables :inherit-env t))
    (setopt agent-shell-openai-codex-environment
            (agent-shell-make-environment-variables :inherit-env t))
    (setq agent-shell-preferred-agent-config
          (agent-shell-anthropic-make-claude-code-config))

    (setopt agent-shell-file-completion-enabled t)))

(setup agent-shell-sidebar
  (:defer (:require agent-shell-sidebar))
  ;; (:load-after agent-shell)
  (keymap-global-set "C-c a s" 'agent-shell-sidebar-toggle)
  (keymap-global-set "C-c a f" 'agent-shell-sidebar-toggle-focus))

;; (setup agent-review
;;   (:load-after agent-shell))

(setup ai-code
  ;; (keymap-global-set "C-c a" 'ai-code-menu)
  (:when-loaded
    (ai-code-set-backend 'codex)
    (setq ai-code-backends-infra-terminal-backend 'eat)
    (setq auto-revert-interval 1)
    (with-eval-after-load 'magit
      (ai-code-magit-setup-transients))))

(setup eca)

(provide 'init-ai)
;;; init-ai.el ends here
