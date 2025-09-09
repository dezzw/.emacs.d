;;; init-ui.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup tool-bar (:when-loaded (tool-bar-mode -1)))
(setup scroll-bar (:when-loaded (set-scroll-bar-mode nil)))
(setup tooltip (:when-loaded (:option tooltip-delay 2.5)))
;; Change global font size easily
(setup default-text-scale (:hook-into after-init))
;; Don't scale font on trackpad pinch!
(global-unset-key (kbd "<pinch>"))

;; Better fringe symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000110
   #b00001100
   #b00011000
   #b00110000
   #b00011000
   #b00001100
   #b00000110])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000])

(setup window
  (:also-load lib-window)
  (:global "C-x |" split-window-horizontally-instead
           "C-x _" split-window-vertically-instead
           "C-x 3" (lambda () (interactive)(select-window (split-window-horizontally)))
           "C-x 2" (lambda () (interactive)(select-window (split-window-vertically)))))

(setup frame
  (:when-loaded
    (let ((border '(internal-border-width . 12)))
      (add-to-list 'default-frame-alist border)
      (add-to-list 'initial-frame-alist border))))

(when (or window-system (daemonp))
  (setup panel
    (:option panel-latitude 43.45193874534566
             panel-longitude -80.49129101085033
             panel-path-max-length 35
             panel-min-left-padding 10
             panel-image-file (concat user-emacs-directory "assets/bitmap.png")
             panel-image-width 400
             panel-image-height 169
             panel-title "The best way to predict the future is to invent it.")
    (:face panel-title-face ((t (:inherit font-lock-constant-face :height 1.2 :italic t :family "Operator Mono"))))
    (panel-create-hook)))

(when (or window-system (daemonp))
  (setup faces
    (:also-load lib-face)
    (:hooks window-setup-hook +setup-fonts
            server-after-make-frame-hook +setup-fonts
            after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (+setup-fonts))))
    (+setup-fonts)))

(setup custom
  (:when-loaded
    (:also-load lib-appearance)
    (:global "M-C-8" (lambda () (interactive) (+adjust-opacity nil -2))
             "M-C-7" (lambda () (interactive) (+adjust-opacity nil 2)))
    ;; Don't prompt to confirm theme safety. This avoids problems with
    ;; first-time startup on Emacs > 26.3.
    (:option custom-safe-themes t
             ;; If you don't customize it, this is the theme you get.
             custom-enabled-themes '(rose-pine-night)
             light-theme 'rose-pine-day
             dark-theme 'rose-pine-night)

    (when *is-mac*
      (apply-theme-based-on-appearance)
      (:with-hook ns-system-appearance-change-functions
        (:hook apply-theme-based-on-appearance)))

    (:with-hook window-setup-hook
      (:hook reapply-themes)
      (:hook opacity-dark-theme)
      (:hook set-dividers-and-fringe-color))

    (:with-hook after-make-frame-functions (:hook opacity-dark-theme))
    (:with-hook after-init-hook (:hook reapply-themes))))

(setup hl-line
  (:option hl-line-range-function
           (lambda () (cons (line-end-position)
                            (line-beginning-position 2))))
  (global-hl-line-mode))

(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.2))

(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2))

(setup highlight-parentheses
  (:defer (:require highlight-parentheses))
  (:when-loaded
    (:option highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
             highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
             highlight-parentheses-delay 0.2))
  (:hooks minibuffer-setup-hook highlight-parentheses-minibuffer-setup)
  (:hook-into prog-mode))


(setup nerd-icons (:defer (:require nerd-icons)))

;; (setup window-navigation
;;   (:defer (:require window-navigation))
;;   (:when-loaded (window-navigation-mode)))

(setup zoom
  (:hook-into window-setup server-after-make-frame)
  (:option zoom-size '(0.618 . 0.618)))

(setup popper
  (:global "C-~"   popper-toggle
           "M-~"   popper-cycle
           "C-M-`" popper-toggle-type)
  (:option popper-window-height (lambda (win)
                                  (fit-window-to-buffer
                                   win
                                   (max 26 (floor (frame-height) 2))
                                   26))
           popper-reference-buffers
           '(("\\*Messages\\*"
              "Output\\*$"
              "\\*Async Shell Command\\*"
              help-mode
              compilation-mode)
             ("\\*Help\\*$")
             ("\\*xref\\*$")
             ("\\*chatgpt\\*$")
             ("\\*vterm\\*$")
             ("\\*eshell\\*$")
             ("\\*Org Select\\*$")
             ("\\*Telega User\\*$")
             ("\\*Telegram Chat Info\\*$")
             ("\\*Telegram Message Info\\*$")
             ("\\*Telegram Sticker Set\\*$")
             ("\\*Telegram Notification Messages\\*$")))
  (:defer (popper-mode +1)
          ;; (popper-echo-mode +1)
          (popper-tab-line-mode +1))
  ;; HACK: close popper window with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(setup tab-bar
  (:defer (:require tab-bar))
  (:when-loaded
    (:global "s-t" tab-bar-new-tab
             "s-w" tab-bar-close-tab)
    (:also-load lib-tabbar)
    (:option tab-bar-separator ""
             tab-bar-close-button-show nil
             tab-bar-new-button-show nil
             tab-bar-new-tab-to 'rightmost
             tab-bar-tab-hints t
             tab-bar-show 1
             tab-bar-new-tab-choice "*scratch*"
             tab-bar-select-tab-modifiers '(super)
             tab-bar-tab-name-truncated-max 20
             tab-bar-auto-width nil
             ;; Add spaces for tab-name
             tab-bar-tab-name-function '+tab-bar-tab-name-function
             tab-bar-tab-name-format-function '+tab-bar-tab-name-format-function
             tab-bar-format '(tab-bar-format-tabs
                              tab-bar-format-add-tab
                              tab-bar-format-align-right))))

(setup activities
  (:hooks after-init-hook activities-mode)
  (:hooks after-init-hook activities-tabs-mode)
  (:option edebug-inhibit-emacs-lisp-mode-bindings t
           activities-kill-buffers t)
  (:global
   "C-x C-a C-n" activities-new
   "C-x C-a C-d" activities-define
   "C-x C-a C-a" activities-resume
   "C-x C-a C-s" activities-suspend
   "C-x C-a C-k" activities-kill
   "C-x C-a RET" activities-switch
   "C-x C-a b" activities-switch-buffer
   "C-x C-a g" activities-revert
   "C-x C-a l" activities-list)
  (:when-loaded
    (:after consult
      ;; hide full buffer list (still available with "b" prefix)
      (consult-customize consult--source-buffer
                         ;; :hidden ,(lambda () (activities-current))
                         :enabled (lambda () (not (activities-current)))
                         :default t)
      (defvar consult--source-activities
        `(:name     "Activity Buffers"
                    :narrow   ?a
                    :category buffer
                    :history  buffer-name-history
                    :action   ,#'switch-to-buffer
                    :enabled  ,(lambda () (activities-current))
                    :items
                    ,(lambda ()
                       (consult--buffer-query
                        :predicate (lambda (b)
                                     (memq b (activities-tabs--tab-parameter
                                              'activities-buffer-list
                                              (activities-tabs--tab (activities-current)))))
                        :sort 'visibility
                        :as #'buffer-name)))
        "Consult source for current activity's buffers.")
      (add-to-list 'consult-buffer-sources 'consult--source-activities))))

(setup which-key
  (:hook-into after-init))

(provide 'init-ui)
;;; init-ui.el ends here
