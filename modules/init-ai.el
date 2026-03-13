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

(setup gptel-agent
  (:when-loaded
    (gptel-agent-update)))

(setup agent-shell
  ;; (:also-load lib-agent-shell)
  ;; (keymap-global-set "C-c a s" 'lib-agent-shell-toggle)
  ;; (keymap-global-set "C-c a f" 'lib-agent-shell-toggle-focus)
  (keymap-global-set "C-c a d" 'agent-shell-send-dwim)
  ;; (keymap-global-set "C-c a n" 'lib-agent-shell-new)

  ;; Make agent-shell bookmark-able
  ;; (defun my/agent-shell-bookmark (_bookmark)
  ;;   (agent-shell))
  ;; (add-hook 'agent-shell-mode-hook
  ;;           (lambda ()
  ;;             (setq-local bookmark-make-record-function
  ;;                         (lambda ()
  ;;                           `((handler . my/agent-shell-bookmark))))))
  (:when-loaded
    (setopt agent-shell-show-usage-at-turn-end t)
    (setopt agent-shell-prefer-viewport-interaction t)
    (setopt agent-shell-session-strategy 'prompt)
    (setopt agent-shell-anthropic-claude-environment
            (agent-shell-make-environment-variables :inherit-env t))
    (setopt agent-shell-openai-codex-environment
            (agent-shell-make-environment-variables :inherit-env t))
    (setq agent-shell-preferred-agent-config
          (agent-shell-anthropic-make-claude-code-config))
    (setopt agent-shell-file-completion-enabled t)))

(setup ai-code
  (:when-loaded
    (ai-code-set-backend 'cursor)
    (setq ai-code-backends-infra-terminal-backend 'eat)
    (setq auto-revert-interval 1)
    (with-eval-after-load 'magit
      (ai-code-magit-setup-transients))))

(provide 'init-ai)
;;; init-ai.el ends here
