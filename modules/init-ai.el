;;; init-ai.el --- AI/LLM related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup gptel
  (:option gptel-default-mode 'org-mode
           gptel-model "openrouter/auto")
  (:when-loaded
    (defun read-file-contents (file-path)
      "Read the contents of FILE-PATH and return it as a string."
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))
    (setq gptel-backend
          (gptel-make-openai "NETINT"
            :host "cursor.netint.ca/openrouter/v1"
            :endpoint "/chat/completions"
            :stream t
            :key (auth-source-pick-first-password :host "cursor.netint.ca" :user "netint")
            :models '("openrouter/auto" "openai/gpt-5-chat" "anthropic/claude-sonnet-4" "anthropic/claude-3.7-sonnet")
            )))
   (:with-hook gptel-post-stream-hook
      (:hook (lambda ()(meow-insert-exit)))
      (:hook gptel-auto-scroll))
    (:hooks gptel-post-response-hook gptel-end-of-response))

(setup ai-code-interface
  (global-set-key (kbd "C-c l") #'ai-code-menu)
  (:when-loaded
    (ai-code-set-backend 'claude-code-ide) ;; use claude-code-ide as backend
    (with-eval-after-load 'magit
      (ai-code-magit-setup-transients))))

(setup agent-shell
  (:when-loaded
    (setopt agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables :inherit-env t))))

(setup agent-shell-sidebar
  (:load-after agent-shell)
  (keymap-global-set "C-c a s" 'agent-shell-sidebar-toggle)
  (keymap-global-set "C-c a f" 'agent-shell-sidebar-toggle-focus))

(provide 'init-ai)
;;; init-ai.el ends here
