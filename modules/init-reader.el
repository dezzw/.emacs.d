;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup nov
  (:file-match "\\.epub\\'")
  (:when-loaded
    (:hooks nov-mode-hook +nov-annotate-font-lock)
    (defface +nov-annotate-face
      '((t (:foreground "#86C166")))
      "Face for # in nov-annotate-face."
      :group 'nov-annotate-face)

    (defun +nov-annotate-font-lock ()
      "Set up font-lock for # in +nov-annotate-face."
      (font-lock-add-keywords
       nil
       '(("『\\(\\(?:.\\|\n\\)*?\\)』" . '+nov-annotate-face)))
      (font-lock-flush))))

;; pdf-view-themed-minor
;; Synchronize color filter with the present Emacs theme.
(setup pdf-view
  (:defer (:require pdf-tools)
          (:file-match "\\.PDF\\'"))
  (:when-loaded
    (:with-mode pdf-view-mode
      (:hook pdf-view-themed-minor-mode))))

(setup org-remark
  (:load-after org)
  (:when-loaded
    (:global "C-c i m" org-remark-mark)
    (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))))

(setup org-remark-nov
  (:load-after nov)
  (:when-loaded (org-remark-nov-mode +1)))

;; (setup gptel
;;   (:when-loaded
;;     (:also-load lib-gpt)
;;     (:also-load org)
;;     (:option gptel-api-key (auth-source-pick-first-password
;;                             :host "api.openai.com"
;;                             :user "apikey")
;;              gptel-default-mode 'org-mode
;;              gptel-model 'gpt-4o
;;              gptel-stream t
;;              gptel-host "api.openai.com"
;;              ;; gptel-proxy "socks://127.0.0.1:7891"
;;              gptel-proxy ""
;;              gptel-directives (get-gptel-directives)
;;              gptel-temperature 0.7
;;              gptel-tools +gptel-tools)

;;     ;; (gptel-make-gemini "Gemini" :key (auth-source-pick-first-password :host "api.gemini.com" :user "gemini") :stream t)
;;     ;; (gptel-make-openai "DeepSeek" :host "api.deepseek.com" :endpoint "/chat/completions" :stream t :key (auth-source-pick-first-password :host "api.deepseek.com" :user "deepseek")
;;     ;;                    :models '(deepseek-chat deepseek-reasoner))
;;     (gptel-make-openai "OpenRouter"
;;       :host "openrouter.ai"
;;       :endpoint "/api/v1/chat/completions"
;;       :key (auth-source-pick-first-password :host "api.openrouter.ai" :user "openrouter")
;;       :models '(deepseek/deepseek-chat
;;                 deepseek/deepseek-r1
;;                 qwen/qwen-turbo
;;                 qwen/qwen-plus
;;                 qwen/qwen-max
;;                 openai/chatgpt-4o-latest
;;                 openai/o1
;;                 openai/o3-mini-high
;;                 anthropic/claude-3.7-sonnet:thinking
;;                 anthropic/claude-3.7-sonnet
;;                 anthropic/claude-3-opus
;;                 google/gemini-2.5-pro-exp-03-25:free
;;                 google/gemini-2.5-pro-preview-03-25
;;                 google/gemini-2.0-flash-thinking-exp:free
;;                 google/gemini-2.0-flash-001)
;;       :stream t)

;;     (:with-hook gptel-post-stream-hook
;;       (:hook (lambda ()(meow-insert-exit)))
;;       (:hook gptel-auto-scroll))
;;     (:hooks gptel-post-response-hook gptel-end-of-response
;;             gptel-mode-hook gptel-set-default-directory)))

(setup elfeed
  (:global "C-x w" elfeed)
  (:when-loaded
    (:also-load lib-elfeed)
    (:option elfeed-feeds +elfeed-feeds
             elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)
    (:with-map elfeed-show-mode-map
      (:bind "N" +menu-dwim--org-capture-elfeed-show
             "o" +open-link-with-mpv))
    (:with-map elfeed-search-mode-map (:bind "L" +elfeed-overview))))

(setup markdown-mode
  (:option markdown-command "pandoc --standalone --css=GTD.css"))

(setup md
  (:defer (:require md))
  (:when-loaded
    (:require md-ts-mode)
    (:with-mode md-ts-mode (:hook md-toc-mode))))

(provide 'init-reader)
;;; init-reader.el ends here
