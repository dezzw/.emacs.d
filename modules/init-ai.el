;;; init-ai.el --- AI/LLM related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup copilot-chat
  (:load-after magit)
  (:hooks git-commit-setup-hook copilot-chat-insert-commit-message)
  (:option copilot-chat-model "claude-3.7-sonnet-thought"))

(setup copilot
  (:defer (:require copilot))
  (:hook-into prog-mode)
  (:with-map copilot-mode-map
    (:bind
     "C-e" copilot-accept-completion))
  (:option copilot-indent-offset-warning-disable t))

(provide 'init-ai)
;;; init-ai.el ends here
