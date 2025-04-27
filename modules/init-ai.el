;;; package --- llm-client configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ediff
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-default-model "ollama_chat/qwen2.5-coder:7b")
  ;; (aidermacs-use-architect-mode t)
  ;; (aidermacs-architect-model "ollama_chat/deepseek-r1:8b")
  ;; (aidermacs-editor-model "ollama_chat/qwen2.5-coder:7b")
  ;; (aidermacs-backend 'vterm)
  (aidermacs-backend 'comint)
  :config
  (aidermacs-setup-minor-mode)
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  (add-to-list 'display-buffer-alist
               `("\\*aidermacs.*\\*"
                 (display-buffer-pop-up-window)))
  )

(use-package gptel
  :straight t
  :config
  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models '(deepseek-r1:8b)))          ;List of models

;; (use-package copilot
  ;; :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  ;; :hook prog-mode
  ;; :bind (:map copilot-completion-map
  ;; 	      ("<tab>" . copilot-accept-completion)
  ;; 	      ("TAB" . copilot-accept-completion)))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el")
  :after (request org markdown-mode magit)
  :bind (:map global-map
              ("C-c C-y" . copilot-chat-yank)
              ("C-c M-y" . copilot-chat-yank-pop)
              ("C-c C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))
  :hook (git-commit-setup . copilot-chat-insert-commit-message)
  :custom
  (copilot-chat-model "claude-3.7-sonnet-thought"))

(provide 'init-ai)
;;; init-llm.el ends here
