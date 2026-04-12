;;; init-ui.el --- UI and window management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup window
  (:also-load lib-window)
  (:global-bind "C-x |" 'split-window-horizontally-instead
                "C-x _" 'split-window-vertically-instead
                "C-x 3" (lambda () (interactive) (select-window (split-window-horizontally)))
                "C-x 2" (lambda () (interactive) (select-window (split-window-vertically)))))

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

(setup ace-window
  (:global-bind "C-x o" 'ace-window)
  (:option aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-scope 'frame))

;; Default text scaling only makes sense on graphical frames.
(setup default-text-scale
  (:if-graphic
    (:hook-into after-init)
    ;; Don't scale font on trackpad pinch!
    (global-unset-key (kbd "<pinch>"))))

(setup frame
  (:if-graphic
    (:when-loaded
      (let ((border '(internal-border-width . 12)))
        (add-to-list 'default-frame-alist border)
        (add-to-list 'initial-frame-alist border)))))

(setup window-divider
  (:if-graphic
    (:option window-divider-default-right-width 1
             window-divider-default-bottom-width 0
             window-divider-default-places t)
    (:hook-into window-setup-hook)))

(setup panel
  (:if-graphic
    (:require panel)
    (:option panel-latitude 43.45193874534566
             panel-longitude -80.49129101085033
             panel-path-max-length 35
             panel-min-left-padding 10
             panel-image-file (concat user-emacs-directory "assets/bitmap.png")
             panel-image-width 400
             panel-image-height 169
             panel-title "The best way to predict the future is to invent it.")
    (:when-loaded
      (:face panel-title-face ((t (:inherit font-lock-constant-face :height 1.2 :italic t :family "Operator Mono"))))
      (panel-create-hook))))

(setup faces
  (:if-graphic
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
    ;; Suppress org-level face warnings by ensuring org-faces are defined early
    (unless (facep 'org-level-1)
      (require 'org-faces nil t))
    (:also-load rose-pine)
    (:also-load lib-appearance)
    ;; Don't prompt to confirm theme safety. This avoids problems with
    ;; first-time startup on Emacs > 26.3.
    (:option custom-safe-themes t
             ;; If you don't customize it, this is the theme you get.
             custom-enabled-themes '(rose-pine-night)
             light-theme 'rose-pine-day
             dark-theme 'rose-pine-night)

    (:if-graphic
      (:global-bind "C-M-8" (lambda () (interactive) (+adjust-opacity nil -2))
                    "C-M-7" (lambda () (interactive) (+adjust-opacity nil 2)))
      (when *is-mac*
        (apply-theme-based-on-appearance)
        (:with-hook ns-system-appearance-change-functions
          (:hook apply-theme-based-on-appearance)))
      (:with-hook window-setup-hook
        (:hook reapply-themes)
        (:hook opacity-dark-theme)
        (:hook set-dividers-and-fringe-color))
      (:with-hook after-make-frame-functions (:hook opacity-dark-theme)))

    (:with-hook after-init-hook (:hook reapply-themes))))

(setup hl-line
  (:option hl-line-range-function
           (lambda () (cons (line-end-position)
                            (line-beginning-position 2))))
  (global-hl-line-mode))

(setup paren
  (:face show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  (:when-loaded
    (:option show-paren-when-point-inside-paren t
             show-paren-when-point-in-periphery t
             show-paren-context-when-offscreen t
             show-paren-delay 0.2
             blink-matching-paren-highlight-offscreen t)))

(setup highlight-parentheses
  (:require highlight-parentheses)
  (:when-loaded
    (:option highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
             highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
             highlight-parentheses-delay 0.2))
  (:hooks minibuffer-setup-hook highlight-parentheses-minibuffer-setup)
  (:hook-into prog-mode))


(setup nerd-icons (:require nerd-icons))

(defun +popper-close-window-hack (&rest _)
  "Close popper window via `C-g'."
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p))
             popper-open-popup-alist)
    (let ((window (caar popper-open-popup-alist)))
      (when (window-live-p window)
        (+popper-delete-popup-window window)))))

(setup popper
  (:also-load lib-posframe)
  (:global-bind "C-c w p" 'popper-toggle
                "C-c w n" 'popper-cycle
                "C-c w P" 'popper-toggle-type)
  (:option popper-window-height (lambda (win)
                                  (fit-window-to-buffer
                                   win
                                   (max 15 (floor (frame-height) 3))
                                   15))
           popper-reference-buffers
           '(;; Shell/build output
             "\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             help-mode
             compilation-mode
             ;; Help and navigation
             "\\*Help\\*$"
             "\\*xref\\*$"
             ;; AI/Chat
             "\\*chatgpt\\*$"
             ;; Terminal emulators
             "\\*vterm\\*$"
             "\\*.*-vterm\\*$"
             "\\*ghostel\\*$"
             "\\*.*-ghostel\\*$"
             "\\*eat\\*$"
             "\\*eshell\\*$"
             "\\*.*-eshell\\*$"
             ;; Org mode
             "\\*Org Select\\*$"
             ;; Verb/restclient HTTP responses
             "\\*HTTP Response.*\\*$"
             ;; Telegram buffers
             "\\*Telega User\\*$"
             "\\*Telegram Chat Info\\*$"
             "\\*Telegram Message Info\\*$"
             "\\*Telegram Sticker Set\\*$"
             "\\*Telegram Notification Messages\\*$")
           popper-display-function #'+popper-display-posframe)
  (setq popper-display-control t)
  (:with-hook after-init-hook
    (:hook popper-mode)
    (:hook popper-tab-line-mode))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(setup tab-bar
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
                            tab-bar-format-align-right))
  (:also-load lib-tabbar)
  (when (fboundp 'tab-bar--update-tab-bar-lines)
    (tab-bar--update-tab-bar-lines)))
(setup tab-line
  (:set tab-line-new-button-show nil
        tab-line-close-button-show nil))

(setup tabspaces
  (:also-load lib-tabspaces)
  (setopt tabspaces-use-filtered-buffers-as-default t
          tabspaces-default-tab "Default"
          tabspaces-remove-to-default t
          tabspaces-include-buffers '("*scratch*")
          tabspaces-initialize-project-with-todo t
          tabspaces-todo-file-name "project-todo.org"
          tabspaces-session t
          tabspaces-session-auto-restore nil
          tabspaces-fully-resolve-paths t
          tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*")
          tab-bar-new-tab-choice "*scratch*")
  (tabspaces-mode)
  (:after consult
    (+tabspaces-consult-setup)))

(setup which-key
  (:with-hook after-init-hook
    (:hook which-key-mode)))

(provide 'init-ui)
;;; init-ui.el ends here
