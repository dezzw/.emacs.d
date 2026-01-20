;;; miniline.el --- Overlay tray with mode-line-format support  -*- lexical-binding: t; -*-
;; Copyright (C) 2026

;; Author: demacs
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A lightweight overlay-based tray for the echo area.
;;
;; Goals:
;; - Use overlays (like awesome-tray) rather than resizing minibuffer buffers.
;; - Fully support `mode-line-format' style specs via `format-mode-line'.
;; - Keep reasonable performance via caching and throttled updates.
;;
;; This library is intentionally minimal: it renders left/right specs and
;; inserts them as an `after-string' overlay in echo area buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'timer)
(require 'minibuffer)
(require 'overlay)

(defgroup miniline nil
  "Overlay tray with `mode-line-format' compatibility."
  :group 'convenience
  :prefix "miniline-")

(defcustom miniline-format nil
  "Format for the miniline tray, same format as `mode-line-format'.

If nil, falls back to the current buffer's `mode-line-format',
which respects whatever packages (doom-modeline, eca, etc.) have set.

Buffer-local `mode-line-format' takes priority over this setting,
allowing packages to define their own display."
  :type 'sexp
  :group 'miniline)

(defcustom miniline-position 'right
  "Tray position.

- right: align to right fringe
- left: align to left fringe
- center: center in echo area"
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Center" center))
  :group 'miniline)

(defcustom miniline-second-line nil
  "If non-nil, display tray in a second line in echo area buffers."
  :type 'boolean
  :group 'miniline)

(defcustom miniline-minibuffer t
  "If non-nil, also display tray when minibuffer is active.

This uses an overlay inside the minibuffer buffer during minibuffer sessions."
  :type 'boolean
  :group 'miniline)

(defcustom miniline-update-interval 0.5
  "Interval in seconds between updating the miniline contents.

If nil, don't update automatically (not recommended)."
  :type 'number
  :group 'miniline)

(defcustom miniline-right-padding 0
  "Extra padding at the right side to avoid wrap." 
  :type 'integer
  :group 'miniline)

(defcustom miniline-hide-mode-line t
  "If non-nil, hide the original mode-line when `miniline-mode' is active."
  :type 'boolean
  :group 'miniline)

(defcustom miniline-display-gui-line t
  "If non-nil, display a thin line at the bottom of windows in GUI mode.
Only effective when `miniline-hide-mode-line' is non-nil."
  :type 'boolean
  :group 'miniline)

(defface miniline-mode-line
  '((((background light))
     :background "#ffffff" :box nil)
    (t
     :background "#ffffff" :box nil))
  "Face for active window's thin mode-line when hidden."
  :group 'miniline)

(defface miniline-mode-line-inactive
  '((((background light))
     :background "#dddddd" :box nil)
    (t
     :background "#444444" :box nil))
  "Face for inactive window's thin mode-line when hidden."
  :group 'miniline)

(defvar miniline--overlays nil
  "List of overlays currently used to display the tray.")

(defvar miniline--text nil
  "The text currently displayed in the miniline.")

(defvar miniline--update-timer nil
  "Timer for periodic updates.")

;; Saved state for restoring mode-line
(defvar miniline--saved-mode-line-format nil
  "Saved default `mode-line-format' before hiding.")
(defvar miniline--saved-mode-line-face nil
  "Saved `mode-line' face attributes.")
(defvar miniline--saved-mode-line-inactive-face nil
  "Saved `mode-line-inactive' face attributes.")
(defvar miniline--saved-mode-line-face-spec nil
  "Saved `mode-line' face spec for restoration.")
(defvar miniline--saved-mode-line-inactive-face-spec nil
  "Saved `mode-line-inactive' face spec for restoration.")
(defvar-local miniline--orig-mode-line-format nil
  "Buffer-local saved `mode-line-format'.")

(defun miniline--get-frame-width ()
  "Return the echo area's effective width." 
  (window-width (minibuffer-window)))

(defun miniline--ensure-echo-overlays ()
  "Create overlays in each echo area buffer." 
  (setq miniline--overlays nil)
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (remove-overlays (point-min) (point-max))
      (push (make-overlay (point-min) (point-max) nil nil t)
            miniline--overlays))))

(defun miniline--minibuffer-setup ()
  "Setup miniline overlay in the active minibuffer." 
  (push (make-overlay (point-max) (point-max) nil t t) miniline--overlays)
  (overlay-put (car miniline--overlays) 'priority 1)
  (miniline-update))

(defun miniline--format (spec)
  "Safely render SPEC using `format-mode-line'." 
  (with-demoted-errors "miniline: %S"
    (when spec
      (format-mode-line spec))))

(defun miniline--align (text)
  "Return TEXT prefixed with an alignment display property.
Dynamically calculates position to avoid wrapping."
  (let* ((text-width (string-width text))
         (frame-width (miniline--get-frame-width))
         ;; Add safety margin to prevent wrapping
         (safety-margin (+ miniline-right-padding 2))
         (wid (+ text-width safety-margin))
         (spc (pcase miniline-position
                ('center (propertize " " 'cursor 1 'display
                                     `(space :align-to (- center ,(/ text-width 2)))))
                ('left (propertize " " 'cursor 1 'display
                                   '(space :align-to left)))
                ;; Right: use exact column calculation for precision
                (_ (let ((target-col (max 0 (- frame-width wid))))
                     (propertize " " 'cursor 1 'display
                                 `(space :align-to ,target-col)))))))
    (concat (if miniline-second-line "\n" "") spc text)))

(defun miniline--set-text (text)
  "Set the text displayed by miniline to TEXT.
Like awesome-tray: updates overlays and writes to *Minibuf-0*."
  ;; Don't update if minibuffer is active (user is typing)
  (unless (active-minibuffer-window)
    (setq miniline--text text)
    
    ;; Prune dead overlays
    (while (and miniline--overlays
                (null (overlay-buffer (car miniline--overlays))))
      (pop miniline--overlays))
    
    ;; Update all overlays
    (dolist (o miniline--overlays)
      (when (overlay-buffer o)
        (overlay-put o 'after-string text)))
    
    ;; Also write to *Minibuf-0* to ensure persistence (like awesome-tray)
    (with-current-buffer " *Minibuf-0*"
      (delete-region (point-min) (point-max))
      (insert text))))

(defun miniline--hidden-format-p (fmt)
  "Return non-nil if FMT is a hidden/empty mode-line format."
  (or (null fmt)
      (equal fmt '(""))
      (equal fmt "")))

(defun miniline--get-buffer-local-format ()
  "Get buffer-local mode-line-format if meaningful."
  (and (local-variable-p 'mode-line-format)
       (not (miniline--hidden-format-p mode-line-format))
       mode-line-format))

(defun miniline--get-default-format ()
  "Get the default miniline format."
  (or miniline-format
      miniline--orig-mode-line-format
      miniline--saved-mode-line-format
      (and (boundp 'minimal-emacs--default-mode-line-format)
           minimal-emacs--default-mode-line-format)))

(defun miniline--remove-spacers (str)
  "Remove space/alignment display properties from STR.
Filters out `(space :align-to ...)' and similar constructs that cause overflow."
  (when (and str (> (length str) 0))
    (let ((result (copy-sequence str))
          (i 0)
          (len (length str)))
      (while (< i len)
        (let ((display (get-text-property i 'display str)))
          (when (and display
                     (or (and (consp display)
                              (or (eq (car display) 'space)
                                  (and (consp (car display))
                                       (eq (caar display) 'space))))
                         (and (listp display)
                              (plist-get display :align-to))
                         (and (listp display)
                              (plist-get display :width))))
            ;; Remove this display property
            (remove-text-properties i (1+ i) '(display nil) result)))
        (setq i (1+ i)))
      ;; Also collapse multiple spaces into single space
      (replace-regexp-in-string "  +" " " (string-trim result)))))

(defun miniline--compose ()
  "Compose tray string with smart merging.
If buffer has a local mode-line-format, merge it with the default format.
Removes spacer/alignment properties to prevent overflow."
  ;; Run in context of selected window's buffer
  (with-current-buffer (window-buffer (selected-window))
    (let* ((buffer-fmt (miniline--get-buffer-local-format))
           (default-fmt (miniline--get-default-format))
           ;; Render and clean both formats (remove spacers)
           (buffer-str (when buffer-fmt
                         (miniline--remove-spacers (or (miniline--format buffer-fmt) ""))))
           (default-str (miniline--remove-spacers (or (miniline--format default-fmt) "")))
           ;; Smart merge: if buffer has custom format, show both with separator
           (tray (cond
                  ;; Both exist and non-empty: merge with separator
                  ((and buffer-str (not (string-empty-p buffer-str))
                        default-str (not (string-empty-p default-str)))
                   (concat default-str "  │  " buffer-str))
                  ;; Only buffer-local format
                  ((and buffer-str (not (string-empty-p buffer-str)))
                   buffer-str)
                  ;; Only default format (may be empty too)
                  ((and default-str (not (string-empty-p default-str)))
                   default-str)
                  (t "")))
           (echo-message (current-message))
           (minibuf-info (if (stringp echo-message) (substring-no-properties echo-message) ""))
           (minibuf-last-line (car (last (split-string minibuf-info "\n"))))
           (blank-length (- (miniline--get-frame-width)
                            (string-width tray)
                            (string-width minibuf-last-line))))
      ;; Align if there's space
      (if (> blank-length 0)
          (miniline--align tray)
        (concat (if miniline-second-line "\n" "") tray)))))

(defun miniline-update ()
  "Update miniline display. Called by timer.
This is the main update function, like `awesome-tray-update'."
  (interactive)
  (condition-case err
      (miniline--set-text (miniline--compose))
    (error (message "miniline error: %S" err))))



(defun miniline--hide-mode-line ()
  "Hide the original mode-line, optionally showing a thin GUI line."
  (when miniline-hide-mode-line
    ;; Save default mode-line-format
    ;; Check for early-init.el saved value first (minimal-emacs optimization)
    (setq miniline--saved-mode-line-format
          (or (and (boundp 'minimal-emacs--default-mode-line-format)
                   minimal-emacs--default-mode-line-format)
              (default-value 'mode-line-format)))
    ;; Save full face specs for proper restoration
    (setq miniline--saved-mode-line-face-spec (get 'mode-line 'face-defface-spec))
    (setq miniline--saved-mode-line-inactive-face-spec (get 'mode-line-inactive 'face-defface-spec))
    ;; Save face attributes (all relevant attributes)
    (setq miniline--saved-mode-line-face
          (list :foreground (face-attribute 'mode-line :foreground nil t)
                :background (face-attribute 'mode-line :background nil t)
                :height (face-attribute 'mode-line :height nil t)
                :box (face-attribute 'mode-line :box nil t)
                :underline (face-attribute 'mode-line :underline nil t)
                :overline (face-attribute 'mode-line :overline nil t)))
    (setq miniline--saved-mode-line-inactive-face
          (list :foreground (face-attribute 'mode-line-inactive :foreground nil t)
                :background (face-attribute 'mode-line-inactive :background nil t)
                :height (face-attribute 'mode-line-inactive :height nil t)
                :box (face-attribute 'mode-line-inactive :box nil t)
                :underline (face-attribute 'mode-line-inactive :underline nil t)
                :overline (face-attribute 'mode-line-inactive :overline nil t)))

    ;; Set default mode-line-format to minimal or nil
    (setq-default mode-line-format
                  (if (and miniline-display-gui-line (display-graphic-p))
                      '("")
                    nil))

    ;; Update all existing buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (local-variable-p 'mode-line-format)
          (setq miniline--orig-mode-line-format mode-line-format)
          (setq mode-line-format
                (if (and miniline-display-gui-line (display-graphic-p))
                    '("")
                  nil)))))

    ;; Apply mode-line face settings
    (miniline--apply-mode-line-faces)))

(defun miniline--restore-mode-line ()
  "Restore the original mode-line."
  (when miniline-hide-mode-line
    ;; Restore default mode-line-format
    (when miniline--saved-mode-line-format
      (setq-default mode-line-format miniline--saved-mode-line-format))

    ;; Restore all buffer-local mode-line-formats
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when miniline--orig-mode-line-format
          (setq mode-line-format miniline--orig-mode-line-format)
          (setq miniline--orig-mode-line-format nil))))

    ;; Restore face specs first (this resets to theme defaults)
    (when miniline--saved-mode-line-face-spec
      (face-spec-set 'mode-line miniline--saved-mode-line-face-spec))
    (when miniline--saved-mode-line-inactive-face-spec
      (face-spec-set 'mode-line-inactive miniline--saved-mode-line-inactive-face-spec))

    ;; Then restore any custom attributes on top
    (when miniline--saved-mode-line-face
      (cl-loop for (attr val) on miniline--saved-mode-line-face by #'cddr
               when (and val (not (eq val 'unspecified)))
               do (set-face-attribute 'mode-line nil attr val)))
    (when miniline--saved-mode-line-inactive-face
      (cl-loop for (attr val) on miniline--saved-mode-line-inactive-face by #'cddr
               when (and val (not (eq val 'unspecified)))
               do (set-face-attribute 'mode-line-inactive nil attr val)))

    (setq miniline--saved-mode-line-format nil
          miniline--saved-mode-line-face nil
          miniline--saved-mode-line-inactive-face nil
          miniline--saved-mode-line-face-spec nil
          miniline--saved-mode-line-inactive-face-spec nil)))

(defun miniline--apply-mode-line-faces ()
  "Apply the mode-line face settings to hide the mode-line.
This can be called to re-apply after theme changes."
  (when (and miniline-mode miniline-hide-mode-line)
    (if (and miniline-display-gui-line (display-graphic-p))
        (let ((active-bg (face-background 'miniline-mode-line nil t))
              (inactive-bg (face-background 'miniline-mode-line-inactive nil t)))
          (face-spec-reset-face 'mode-line)
          (face-spec-reset-face 'mode-line-inactive)
          (set-face-attribute 'mode-line nil
                              :foreground active-bg
                              :background active-bg
                              :height 10
                              :box nil
                              :underline nil
                              :overline nil
                              :inverse-video nil)
          (set-face-attribute 'mode-line-inactive nil
                              :foreground inactive-bg
                              :background inactive-bg
                              :height 10
                              :box nil
                              :underline nil
                              :overline nil
                              :inverse-video nil))
      (face-spec-reset-face 'mode-line)
      (face-spec-reset-face 'mode-line-inactive)
      (set-face-attribute 'mode-line nil
                          :foreground (face-background 'default)
                          :background (face-background 'default)
                          :height 10
                          :box nil
                          :underline nil
                          :overline nil
                          :inverse-video nil)
      (set-face-attribute 'mode-line-inactive nil
                          :foreground (face-background 'default)
                          :background (face-background 'default)
                          :height 10
                          :box nil
                          :underline nil
                          :overline nil
                          :inverse-video nil))))

(defun miniline--after-load-theme (&rest _)
  "Re-apply mode-line hiding after a theme is loaded."
  (miniline--apply-mode-line-faces))

(defun miniline--enable ()
  "Enable miniline mode."
  (miniline--hide-mode-line)
  (miniline--ensure-echo-overlays)
  
  ;; Setup minibuffer hook
  (when miniline-minibuffer
    (add-hook 'minibuffer-setup-hook #'miniline--minibuffer-setup))

  ;; Re-apply face settings after theme changes
  (advice-add 'load-theme :after #'miniline--after-load-theme)
  (advice-add 'enable-theme :after #'miniline--after-load-theme)
  ;; Also handle after-init for themes loaded during init
  (add-hook 'after-init-hook #'miniline--apply-mode-line-faces)
  ;; And when new frames are created
  (add-hook 'after-make-frame-functions (lambda (_) (miniline--apply-mode-line-faces)))

  ;; Start the timer to automatically update (like awesome-tray)
  (when miniline-update-interval
    (setq miniline--update-timer
          (run-with-timer 0 miniline-update-interval #'miniline-update)))

  ;; Initial update
  (miniline-update))

(defun miniline--disable ()
  "Disable miniline mode."
  (remove-hook 'minibuffer-setup-hook #'miniline--minibuffer-setup)
  (remove-hook 'after-init-hook #'miniline--apply-mode-line-faces)
  (remove-hook 'after-make-frame-functions (lambda (_) (miniline--apply-mode-line-faces)))
  (advice-remove 'load-theme #'miniline--after-load-theme)
  (advice-remove 'enable-theme #'miniline--after-load-theme)

  ;; Cancel the update timer
  (cancel-function-timers #'miniline-update)
  (when (timerp miniline--update-timer)
    (cancel-timer miniline--update-timer))
  (setq miniline--update-timer nil)

  ;; Remove overlays
  (mapc #'delete-overlay miniline--overlays)
  (setq miniline--overlays nil)
  
  ;; Clear text from *Minibuf-0*
  (with-current-buffer " *Minibuf-0*"
    (delete-region (point-min) (point-max)))
  
  (setq miniline--text nil)

  (miniline--restore-mode-line))

;;;###autoload
(define-minor-mode miniline-mode
  "Display `mode-line-format' info in the echo area using overlays." 
  :global t
  (if miniline-mode
      (miniline--enable)
    (miniline--disable)))

(provide 'miniline)
;;; miniline.el ends here
