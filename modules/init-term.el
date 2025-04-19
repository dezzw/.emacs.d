;;; init-term.el --- all kinds of terms related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package eshell
  ;; :ensure nil
  :commands (eshell)
  :config
  (setq eshell-directory-name (concat user-emacs-directory "eshell/"))
  
  (if (executable-find "exa")
      (defalias 'eshell/ls 'exa)))

(use-package eshell-prompt-extras
  :straight t
  :after esh-opt
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-up
  :straight t
  :after esh-mode
  :custom
  (eshell-up-ignore-case nil))

(use-package eshell-syntax-highlighting
  :straight t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-z
  :straight t
  :after esh-mode)

(use-package esh-help
  :straight t
  :after esh-mode
  :config
  (setup-esh-help-eldoc))


(use-package eat
  :straight '(eat :type git
		  :host codeberg
		  :repo "akib/emacs-eat"
		  :files ("*.el" ("term" "term/*.el") "*.texi"
			  "*.ti" ("terminfo/e" "terminfo/e/*")
			  ("terminfo/65" "terminfo/65/*")
			  ("integration" "integration/*")
			  (:exclude ".dir-locals.el" "*-tests.el")))
  :commands (eat eshell)
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;; Copy from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el
;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(use-package vterm
  :straight t
  :commands (vterm vterm-posframe-toggle)
  :bind ("C-c `" . vterm-posframe-toggle)
  :custom
  (vterm-max-scrollback 10000)
  :config
  (with-no-warnings
    (when (and (fboundp #'posframe-workable-p)
	       (posframe-workable-p))
      (defvar vterm-posframe--frame nil)
      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let ((buffer (vterm--internal #'ignore 100))
              (width  (max 80 (/ (frame-width) 2)))
              (height (/ (frame-height) 2)))
          (if (frame-live-p vterm-posframe--frame)
              (progn
                (posframe-delete-frame buffer)
                (setq vterm-posframe--frame nil))
            (setq vterm-posframe--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :accept-focus t))))))))

(use-package multi-vterm
  :straight t
  :after vterm
  :commands (multi-vterm))

(use-package vterm-toggle
  :straight t
  :after vterm
  :commands  (vterm-toggle-cd))


(provide 'init-term)
;;; init-eshell.el ends here
