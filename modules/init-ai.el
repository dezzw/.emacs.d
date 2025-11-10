;;; init-ai.el --- AI/LLM related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; install claude-code.el, using :depth 1 to reduce download size:
(setup claude-code
  (:pkg (claude-code :type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*"))))
  (:pkg (monet :type git :host github :repo "stevemolitor/monet"))
  (:when-loaded
    (keymap-global-set "C-c c" claude-code-command-map)
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
    (monet-mode 1)

    (claude-code-mode)))

(setup ai-code-interface
  (:pkg claude-code-ide )
  (:pkg (ai-code-interface :host github :repo "tninja/ai-code-interface.el"))
  (global-set-key (kbd "C-c l") #'ai-code-menu)
  (:when-loaded
    (ai-code-set-backend  'claude-code-ide) ;; use claude-code-ide as backend
    (with-eval-after-load 'magit
      (ai-code-magit-setup-transients))))
  
(provide 'init-ai)
;;; init-ai.el ends here
