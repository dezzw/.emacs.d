;;; -*- lexical-binding: t -*-

(use-package tab-bar
  :straight nil
  :when (display-graphic-p)
  :hook (window-setup . tab-bar-mode)
  :custom
  (tab-bar-separator "")
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-hints nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-auto-width nil)
  ;; Add spaces for tab-name
  (tab-bar-tab-name-function '+tab-bar-tab-name-function)
  (tab-bar-tab-name-format-function '+tab-bar-tab-name-format-function)
  (tab-bar-format '(tab-bar-format-menu-bar
                    tab-bar-format-tabs
                    tab-bar-format-add-tab
                    tab-bar-format-align-right))
  :config
  (defun tab-bar-format-menu-bar ()
    "Produce the Menu button for the tab bar that shows the menu bar."
    `((menu-bar menu-item
		(format " %s "
			(nerd-icons-sucicon "nf-custom-emacs"
                                            :face '(:inherit nerd-icons-purple)))
		tab-bar-menu-bar :help "Menu Bar")))

  (defun +tab-bar-tab-name-function ()
    (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
           (count (length (window-list-1 nil 'nomini)))
           (truncated-tab-name (if (< (length raw-tab-name)
                                      tab-bar-tab-name-truncated-max)
                                   raw-tab-name
				 (truncate-string-to-width raw-tab-name
                                                           tab-bar-tab-name-truncated-max
                                                           nil nil tab-bar-tab-name-ellipsis))))
      (if (> count 1)
          (concat truncated-tab-name "(" (number-to-string count) ")")
	truncated-tab-name)))

  (defun +tab-bar-tab-name-format-function (tab i)
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (concat
       ;; change tab-bar's height
       (propertize " " 'display '(raise 0.25))
       ;; (propertize (format "%d:" i) 'face `(:inherit ,face :weight ultra-bold))
       (propertize (concat " " (alist-get 'name tab) " ") 'face face)
       (propertize " " 'display '(raise -0.25))))))


;; (use-package tabspaces
;;   :straight (:type git :host github :repo "mclear-tools/tabspaces")
;;   :when (display-graphic-p)
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*" "*Messages*"))
;;   ;; sessions
;;   ;; (tabspaces-session t)
;;   ;; (tabspaces-session-auto-restore t)

;;   :config
;;   ;; Filter Buffers for Consult-Buffer
;;   (with-eval-after-load 'consult
;;     ;; hide full buffer list (still available with "b" prefix)
;;     (consult-customize consult--source-buffer :hidden t :default nil)
;;     ;; set consult-workspace buffer list
;;     (defvar consult--source-workspace
;;       (list :name     "Workspace Buffer"
;;             :narrow   ?w
;;             :history  'buffer-name-history
;;             :category 'buffer
;;             :state    #'consult--buffer-state
;;             :default  t
;;             :items    (lambda () (consult--buffer-query
;; 				  :predicate #'tabspaces--local-buffer-p
;; 				  :sort 'visibility
;; 				  :as #'buffer-name)))

;;       "Set workspace buffer list for consult-buffer.")
;;     (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package easysession
  :straight t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  :config
  ;; kill all buffers before loading a session
  (defun kill-old-session-buffers ()
    (save-some-buffers t)
    (mapc #'kill-buffer
          (cl-remove-if
           (lambda (buffer)
             (string= (buffer-name buffer) "*Messages*"))
           (buffer-list)))
    (delete-other-windows))
  (add-hook 'easysession-before-load-hook #'kill-old-session-buffers)
  (add-hook 'easysession-new-session-hook #'kill-old-session-buffers)
  ;; handle Emacs daemon mode
  (when (daemonp)
    (defun my-setup-easy-session ()
      (easysession-load-including-geometry)
      (easysession-save-mode)
      (remove-hook 'server-after-make-frame-hook #'my-setup-easy-session))

    (add-hook 'server-after-make-frame-hook #'my-setup-easy-session)))

(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config (winner-mode 1))

(use-package popper
  :straight t
  :bind (:map popper-mode-map
              ("C-h z"       . popper-toggle)
              ("C-<tab>"     . popper-cycle)
              ("C-M-<tab>"   . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
	  helpful-mode
          compilation-mode
          ;; "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          )))

(provide 'init-wm)
