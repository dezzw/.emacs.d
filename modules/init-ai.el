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
  (:global-bind "C-c a d" 'agent-shell-send-dwim)
  (:when-loaded
    (setopt agent-shell-show-usage-at-turn-end t
            agent-shell-prefer-viewport-interaction t
            agent-shell-session-strategy 'prompt
            agent-shell-anthropic-claude-environment
            (agent-shell-make-environment-variables :inherit-env t)
            agent-shell-openai-codex-environment
            (agent-shell-make-environment-variables :inherit-env t)
            agent-shell-openai-authentication
            (agent-shell-openai-make-authentication :api-key (lambda () (getenv "OPENAI_API_KEY")))
            agent-shell-file-completion-enabled t)
    (setq agent-shell-preferred-agent-config
          (agent-shell-anthropic-make-claude-code-config))))


(setup anvil
  (unless (package-installed-p 'anvil)
    (package-vc-install
     '(anvil :url "https://github.com/zawatton/anvil.el.git")))
  (:when-loaded
    (anvil-enable)
    (anvil-server-start)))

(provide 'init-ai)
;;; init-ai.el ends here
