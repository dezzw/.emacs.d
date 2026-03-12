;;; lib-agent-shell.el --- Side-window helpers for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:

;; Keeps agent-shell in a side window while delegating session, viewport,
;; config, and project behavior to upstream agent-shell.

;;; Code:

(require 'agent-shell)
(require 'seq)

(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")

(defvar lib-agent-shell-side 'right
  "Side used for agent-shell windows.")

(defvar lib-agent-shell-width 90
  "Width in columns used for agent-shell windows.")

(defvar lib-agent-shell-slot 0
  "Side window slot used for agent-shell windows.")

(defvar lib-agent-shell-lock-window t
  "When non-nil, dedicate the sidebar window and skip it in window cycling.")

(defvar lib-agent-shell--last-window nil
  "Last non-agent window selected before entering agent-shell.")

(defun lib-agent-shell-display-action ()
  "Return the display action used for agent-shell side windows."
  `(display-buffer-in-side-window
    (side . ,lib-agent-shell-side)
    (slot . ,lib-agent-shell-slot)
    (window-width . ,lib-agent-shell-width)
    (dedicated . ,lib-agent-shell-lock-window)
    (window-parameters . ((no-delete-other-windows . t)
                          (no-other-window . ,lib-agent-shell-lock-window)))))

(setq agent-shell-display-action (lib-agent-shell-display-action))

(defun lib-agent-shell--agent-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an agent-shell shell or viewport buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (derived-mode-p 'agent-shell-mode)
        (derived-mode-p 'agent-shell-viewport-view-mode)
        (derived-mode-p 'agent-shell-viewport-edit-mode))))

(defun lib-agent-shell--current-project-shell-buffer ()
  "Return the current project's shell buffer, if any."
  (or (and (derived-mode-p 'agent-shell-mode)
           (current-buffer))
      (and (or (derived-mode-p 'agent-shell-viewport-view-mode)
               (derived-mode-p 'agent-shell-viewport-edit-mode))
           (agent-shell-viewport--shell-buffer (current-buffer)))
      (seq-first (agent-shell-project-buffers))))

(defun lib-agent-shell--current-project-window ()
  "Return the visible agent-shell window for the current project, if any."
  (when-let* ((shell-buffer (lib-agent-shell--current-project-shell-buffer)))
    (or (when agent-shell-prefer-viewport-interaction
          (when-let* ((viewport-buffer
                       (agent-shell-viewport--buffer
                        :shell-buffer shell-buffer
                        :existing-only t)))
            (get-buffer-window viewport-buffer)))
        (get-buffer-window shell-buffer))))

(defun lib-agent-shell--remember-window ()
  "Remember the currently selected non-agent window."
  (unless (lib-agent-shell--agent-buffer-p)
    (setq lib-agent-shell--last-window (selected-window))))

(defun lib-agent-shell--restore-window ()
  "Restore focus to the most recently remembered non-agent window."
  (when (and lib-agent-shell--last-window
             (window-live-p lib-agent-shell--last-window))
    (select-window lib-agent-shell--last-window)))

(defun lib-agent-shell-toggle ()
  "Toggle the current project's agent-shell side window."
  (interactive)
  (if-let* ((window (lib-agent-shell--current-project-window)))
      (progn
        (delete-window window)
        (lib-agent-shell--restore-window))
    (lib-agent-shell--remember-window)
    (condition-case nil
        (agent-shell-toggle)
      (user-error
       (agent-shell)))))

(defun lib-agent-shell-toggle-focus ()
  "Toggle focus between the current project's agent-shell and prior window."
  (interactive)
  (cond
   ((lib-agent-shell--agent-buffer-p)
    (lib-agent-shell--restore-window))
   ((lib-agent-shell--current-project-window)
    (lib-agent-shell--remember-window)
    (select-window (lib-agent-shell--current-project-window)))
   (t
    (lib-agent-shell--remember-window)
    (condition-case nil
        (agent-shell-toggle)
      (user-error
       (agent-shell))))))

(defun lib-agent-shell-new ()
  "Start a new agent-shell session in the side window."
  (interactive)
  (lib-agent-shell--remember-window)
  (agent-shell '(4)))

(provide 'lib-agent-shell)

;;; lib-agent-shell.el ends here
