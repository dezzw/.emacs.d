;;; miniline-segments.el --- Segments for miniline  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: demacs
;; Keywords: convenience

;;; Commentary:

;; Modular segments for miniline, inspired by doom-modeline.
;; Each segment is a function that returns a string (or nil).
;; Segments use caching where appropriate for performance.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(eval-when-compile
  (require 'flymake nil t)
  (require 'flycheck nil t))

;;; Customization

(defgroup miniline-segments nil
  "Segments for miniline."
  :group 'miniline
  :prefix "miniline-segment-")

(defcustom miniline-segment-icon t
  "Whether to display icons in segments.
Requires `nerd-icons' to be installed."
  :type 'boolean
  :group 'miniline-segments)

(defcustom miniline-segment-separator " "
  "Separator between segments."
  :type 'string
  :group 'miniline-segments)

;;; Faces

(defface miniline-buffer-modified
  '((t :inherit error))
  "Face for modified buffer indicator."
  :group 'miniline-segments)

(defface miniline-buffer-read-only
  '((t :inherit warning))
  "Face for read-only buffer indicator."
  :group 'miniline-segments)

(defface miniline-buffer-name
  '((t :inherit mode-line-buffer-id :weight bold))
  "Face for buffer name."
  :group 'miniline-segments)

(defface miniline-major-mode
  '((t :inherit font-lock-keyword-face))
  "Face for major mode."
  :group 'miniline-segments)

(defface miniline-vcs
  '((t :inherit font-lock-function-name-face))
  "Face for VCS/git info."
  :group 'miniline-segments)

(defface miniline-info
  '((t :inherit success))
  "Face for info-level diagnostics."
  :group 'miniline-segments)

(defface miniline-warning
  '((t :inherit warning))
  "Face for warning-level diagnostics."
  :group 'miniline-segments)

(defface miniline-error
  '((t :inherit error))
  "Face for error-level diagnostics."
  :group 'miniline-segments)

(defface miniline-position
  '((t :inherit shadow))
  "Face for position info."
  :group 'miniline-segments)

(defface miniline-remote
  '((t :inherit font-lock-comment-face))
  "Face for remote host indicator."
  :group 'miniline-segments)

(defface miniline-project
  '((t :inherit font-lock-constant-face))
  "Face for project name."
  :group 'miniline-segments)

(defface miniline-lsp
  '((t :inherit success :weight bold))
  "Face for LSP indicator."
  :group 'miniline-segments)

(defface miniline-meow-normal
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for meow normal state."
  :group 'miniline-segments)

(defface miniline-meow-insert
  '((t :inherit font-lock-string-face :weight bold))
  "Face for meow insert state."
  :group 'miniline-segments)

(defface miniline-meow-motion
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for meow motion state."
  :group 'miniline-segments)

;;; Icon helpers

(defun miniline--icon (icon-name fallback &optional face)
  "Return ICON-NAME if `nerd-icons' is available, otherwise FALLBACK.
Optionally apply FACE."
  (let ((icon (if (and miniline-segment-icon
                       (require 'nerd-icons nil t)
                       (fboundp 'nerd-icons-mdicon))
                  (ignore-errors (nerd-icons-mdicon icon-name :face face))
                nil)))
    (if icon icon (if face (propertize fallback 'face face) fallback))))

;;; Segment: Buffer State (modified/read-only)

(defun miniline-segment-buffer-state ()
  "Display buffer read-only state."
  (if buffer-read-only
      (miniline--icon "nf-md-lock" "%%" 'miniline-buffer-read-only)
    ""))

;;; Segment: Buffer Name

(defun miniline-segment-buffer-name ()
  "Display the buffer name."
  (propertize (buffer-name) 'face 'miniline-buffer-name))

;;; Segment: Buffer Info (state + name combined)

(defun miniline-segment-buffer-info ()
  "Display buffer state and name."
  (let ((state (miniline-segment-buffer-state))
        (name (miniline-segment-buffer-name)))
    (if (string-empty-p state)
        name
      (concat state " " name))))

;;; Segment: Major Mode

(defun miniline--major-mode-icon ()
  "Return an icon for the current major mode, or nil if unavailable."
  (when (and miniline-segment-icon
             (require 'nerd-icons nil t)
             (fboundp 'nerd-icons-icon-for-buffer))
    (let ((icon (ignore-errors (nerd-icons-icon-for-buffer))))
      ;; nerd-icons-icon-for-buffer returns a symbol if no icon found
      (when (and icon (stringp icon) (not (string-empty-p icon)))
        icon))))

(defun miniline--major-mode-name ()
  "Return the major mode name without the '-mode' suffix."
  (car (split-string (format "%s" major-mode) "-mode")))

(defun miniline-segment-major-mode ()
  "Display the major mode as an icon (with fallback to text).
Uses `nerd-icons' for the icon if available, otherwise displays
the mode name without the '-mode' suffix."
  (let ((icon (miniline--major-mode-icon))
        (name (miniline--major-mode-name)))
    (if icon
        (propertize icon
                    'face 'miniline-major-mode
                    'help-echo (format "Major mode: %s" name))
      (propertize name 'face 'miniline-major-mode))))

;;; Segment: Position

(defun miniline-segment-position ()
  "Display cursor position (line:column percentage)."
  (propertize (format-mode-line "%l:%c %p%%")
              'face 'miniline-position))

;;; Segment: Remote Host

(defun miniline-segment-remote-host ()
  "Display remote host for TRAMP buffers."
  (when default-directory
    (when-let* ((host (file-remote-p default-directory 'host)))
      (propertize (concat "@" host) 'face 'miniline-remote))))

;;; Segment: Project Name

(defvar-local miniline--project-cache nil)
(defvar-local miniline--project-cache-dir nil)

(defun miniline-segment-project ()
  "Display the current project name."
  (unless (equal default-directory miniline--project-cache-dir)
    (setq miniline--project-cache-dir default-directory)
    (setq miniline--project-cache
          (cond
           ;; project.el
           ((and (fboundp 'project-current)
                 (fboundp 'project-name))
            (when-let* ((proj (project-current)))
              (project-name proj)))
           ;; projectile
           ((and (bound-and-true-p projectile-mode)
                 (fboundp 'projectile-project-name))
            (let ((name (projectile-project-name)))
              (unless (string= name "-") name)))
           (t nil))))
  (when miniline--project-cache
    (propertize (format "[%s]" miniline--project-cache)
                'face 'miniline-project)))

;;; Segment: VCS/Git

(defvar-local miniline--vcs-cache nil)
(defvar-local miniline--vcs-cache-file nil)

(defun miniline--vcs-update-cache ()
  "Update VCS cache for current buffer."
  (setq miniline--vcs-cache-file buffer-file-name)
  (setq miniline--vcs-cache
        (when (and buffer-file-name vc-mode)
          (let* ((backend (vc-backend buffer-file-name))
                 (branch (when backend
                           (substring-no-properties vc-mode
                                                    (+ 2 (length (symbol-name backend)))))))
            (when (and branch (not (string-empty-p branch)))
              (propertize (concat " " branch)
                          'face 'miniline-vcs))))))

(defun miniline-segment-vcs ()
  "Display VCS/Git branch info."
  (unless (equal buffer-file-name miniline--vcs-cache-file)
    (miniline--vcs-update-cache))
  miniline--vcs-cache)

;; Update VCS cache on save
(add-hook 'after-save-hook #'miniline--vcs-update-cache)
(add-hook 'find-file-hook #'miniline--vcs-update-cache)

;;; Segment: Flymake

(defvar-local miniline--flymake-cache nil)

(defun miniline--flymake-update (&rest _)
  "Update flymake cache."
  (setq miniline--flymake-cache
        (when (and (bound-and-true-p flymake-mode)
                   (bound-and-true-p flymake--state))
          (let* ((known (hash-table-keys flymake--state))
                 (running (flymake-running-backends))
                 (disabled (flymake-disabled-backends))
                 (reported (flymake-reporting-backends))
                 (all-disabled (and disabled (null running)))
                 (some-waiting (cl-set-difference running reported)))
            (cond
             (some-waiting (propertize "⏳" 'face 'miniline-warning))
             ((null known) nil)
             (all-disabled (propertize "⚠" 'face 'miniline-warning))
             (t
              (let ((err 0) (warn 0) (note 0))
                (maphash
                 (lambda (_b state)
                   (cl-loop
                    for diag in (flymake--state-diags state) do
                    (let ((severity (flymake--lookup-type-property
                                     (flymake--diag-type diag) 'severity
                                     (warning-numeric-level :error))))
                      (cond ((> severity (warning-numeric-level :warning)) (cl-incf err))
                            ((> severity (warning-numeric-level :debug)) (cl-incf warn))
                            (t (cl-incf note))))))
                 flymake--state)
                (let ((parts nil))
                  (when (> err 0)
                    (push (propertize (format "✖%d" err) 'face 'miniline-error) parts))
                  (when (> warn 0)
                    (push (propertize (format "⚠%d" warn) 'face 'miniline-warning) parts))
                  (when (> note 0)
                    (push (propertize (format "●%d" note) 'face 'miniline-info) parts))
                  (if parts
                      (string-join (nreverse parts) " ")
                    (propertize "✔" 'face 'miniline-info))))))))))

(defun miniline-segment-flymake ()
  "Display flymake diagnostics."
  miniline--flymake-cache)

(with-eval-after-load 'flymake
  (add-hook 'flymake-mode-hook #'miniline--flymake-update)
  (advice-add 'flymake--handle-report :after #'miniline--flymake-update))

;;; Segment: Flycheck

(defvar-local miniline--flycheck-cache nil)

(defun miniline--flycheck-update (&optional _status)
  "Update flycheck cache."
  (setq miniline--flycheck-cache
        (when (bound-and-true-p flycheck-mode)
          (let* ((counts (flycheck-count-errors flycheck-current-errors))
                 (err (or (cdr (assq 'error counts)) 0))
                 (warn (or (cdr (assq 'warning counts)) 0))
                 (info (or (cdr (assq 'info counts)) 0)))
            (let ((parts nil))
              (when (> err 0)
                (push (propertize (format "✖%d" err) 'face 'miniline-error) parts))
              (when (> warn 0)
                (push (propertize (format "⚠%d" warn) 'face 'miniline-warning) parts))
              (when (> info 0)
                (push (propertize (format "●%d" info) 'face 'miniline-info) parts))
              (if parts
                  (string-join (nreverse parts) " ")
                (propertize "✔" 'face 'miniline-info)))))))

(defun miniline-segment-flycheck ()
  "Display flycheck diagnostics."
  miniline--flycheck-cache)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-status-changed-functions #'miniline--flycheck-update)
  (add-hook 'flycheck-mode-hook #'miniline--flycheck-update))

;;; Segment: Checker (flymake or flycheck)

(defun miniline-segment-checker ()
  "Display flymake or flycheck diagnostics, whichever is active."
  (cond
   ((bound-and-true-p flymake-mode) (miniline-segment-flymake))
   ((bound-and-true-p flycheck-mode) (miniline-segment-flycheck))
   (t nil)))

;;; Segment: LSP (eglot or lsp-mode)

(defun miniline-segment-lsp ()
  "Display LSP status indicator."
  (cond
   ;; Eglot
   ((and (bound-and-true-p eglot--managed-mode)
         (fboundp 'eglot-current-server)
         (eglot-current-server))
    (propertize "LSP" 'face 'miniline-lsp))
   ;; lsp-mode
   ((bound-and-true-p lsp-mode)
    (propertize "LSP" 'face 'miniline-lsp))
   (t nil)))

;;; Segment: Meow State

(defun miniline-segment-meow ()
  "Display meow modal editing state."
  (when (bound-and-true-p meow-mode)
    (let ((state (bound-and-true-p meow--current-state)))
      (pcase state
        ('normal (propertize "[N]" 'face 'miniline-meow-normal))
        ('insert (propertize "[I]" 'face 'miniline-meow-insert))
        ('motion (propertize "[M]" 'face 'miniline-meow-motion))
        ('keypad (propertize "[K]" 'face 'miniline-meow-normal))
        ('beacon (propertize "[B]" 'face 'miniline-meow-normal))
        (_ (when-let* ((indicator (bound-and-true-p meow--indicator)))
             (propertize (string-trim indicator) 'face 'miniline-meow-normal)))))))

;;; Segment: Evil State

(defun miniline-segment-evil ()
  "Display evil modal editing state."
  (when (bound-and-true-p evil-local-mode)
    (pcase evil-state
      ('normal (propertize "<N>" 'face 'miniline-meow-normal))
      ('insert (propertize "<I>" 'face 'miniline-meow-insert))
      ('visual (propertize "<V>" 'face 'miniline-warning))
      ('motion (propertize "<M>" 'face 'miniline-meow-motion))
      ('emacs (propertize "<E>" 'face 'miniline-info))
      ('operator (propertize "<O>" 'face 'miniline-warning))
      ('replace (propertize "<R>" 'face 'miniline-error))
      (_ nil))))

;;; Segment: Modal (meow or evil)

(defun miniline-segment-modal ()
  "Display modal editing state (meow or evil)."
  (or (miniline-segment-meow)
      (miniline-segment-evil)))

;;; Segment: Time

(defun miniline-segment-time ()
  "Display current time."
  (propertize (format-time-string "%H:%M") 'face 'shadow))

;;; Segment: Encoding

(defun miniline-segment-encoding ()
  "Display buffer encoding and line ending."
  (let* ((sys (coding-system-plist buffer-file-coding-system))
         (cat (plist-get sys :category))
         (eol (coding-system-eol-type buffer-file-coding-system)))
    (unless (memq cat '(coding-category-undecided coding-category-utf-8))
      (concat (upcase (symbol-name (plist-get sys :name)))
              (pcase eol
                (1 " CRLF")
                (2 " CR")
                (_ ""))))))

;;; Compose segments

(defun miniline-compose-segments (&rest segments)
  "Compose SEGMENTS into a single string, separated by `miniline-segment-separator'.
Each segment is either a string, a function returning a string, or nil."
  (let ((parts
         (cl-remove-if
          (lambda (s) (or (null s) (string-empty-p s)))
          (mapcar (lambda (seg)
                    (cond
                     ((stringp seg) seg)
                     ((functionp seg) (ignore-errors (funcall seg)))
                     (t nil)))
                  segments))))
    (string-join parts miniline-segment-separator)))

;;; Predefined formats

(defvar miniline-format-default
  '(:eval (miniline-compose-segments
           #'miniline-segment-modal
           #'miniline-segment-buffer-info
           #'miniline-segment-remote-host
           #'miniline-segment-vcs
           #'miniline-segment-major-mode
           #'miniline-segment-checker))
  "Default miniline format with common segments.")

(defvar miniline-format-minimal
  '(:eval (miniline-compose-segments
           #'miniline-segment-buffer-info
           #'miniline-segment-major-mode))
  "Minimal miniline format.")

(defvar miniline-format-full
  '(:eval (miniline-compose-segments
           #'miniline-segment-modal
           #'miniline-segment-project
           #'miniline-segment-buffer-info
           #'miniline-segment-position
           #'miniline-segment-remote-host
           #'miniline-segment-vcs
           #'miniline-segment-major-mode
           #'miniline-segment-lsp
           #'miniline-segment-checker
           #'miniline-segment-encoding
           #'miniline-segment-time))
  "Full miniline format with all segments.")

(provide 'miniline-segments)
;;; miniline-segments.el ends here
