;;; lib-popper.el --- Floating popup helpers for Popper -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup +popper-floating-popup nil
  "Floating popup settings for Popper child-frame backend."
  :group 'windows)

(defcustom +popper-posframe-border-color "#7f849c"
  "Border color used by floating Popper child frames."
  :group '+popper-floating-popup
  :type 'string)

(defcustom +popper-floating-popup-move-step 24
  "Pixel step used when moving floating Popper child frames."
  :group '+popper-floating-popup
  :type 'integer)

(defvar-local +popper-floating-popup-mode nil
  "Non-nil when the current buffer is displayed as a floating Popper child frame.")

(defvar +popper-floating-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'+popper-close-floating-popup)
    (define-key map (kbd "Q") #'+popper-close-floating-popup)
    (define-key map [remap quit-window] #'+popper-close-floating-popup)
    (define-key map (kbd "M-<left>") #'+popper-move-floating-popup-left)
    (define-key map (kbd "M-<right>") #'+popper-move-floating-popup-right)
    (define-key map (kbd "M-<up>") #'+popper-move-floating-popup-up)
    (define-key map (kbd "M-<down>") #'+popper-move-floating-popup-down)
    (define-key map (kbd "M-0") #'+popper-center-floating-popup)
    map)
  "Keymap used in floating Popper child-frame buffers.")

(define-minor-mode +popper-floating-popup-mode
  "Minor mode enabled in Popper floating child-frame buffers."
  :lighter nil
  :keymap +popper-floating-popup-mode-map)

(defun +popper-ensure-popup-modeline ()
  "Ensure current popup buffer has a mode-line and no header-line."
  ;; Keep header-line hidden as requested.
  (setq-local header-line-format nil)
  ;; Some buffers disable mode-line entirely; restore a minimal one so
  ;; drag-with-mode-line has a visible drag handle.
  (when (null mode-line-format)
    (setq-local mode-line-format
                '(" " mode-line-buffer-identification "  " mode-name " "))))

(defun +popper-top-level-frame (&optional window)
  "Return the top-level parent frame for WINDOW or the selected frame."
  (let ((frame (window-frame (or window (selected-window)))))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defun +popper-child-frame-window-p (window)
  "Return non-nil when WINDOW belongs to a child frame."
  (frame-parent (window-frame window)))

(defun +popper-current-floating-popup-window ()
  "Return current Popper floating popup window, or nil."
  (let ((window (selected-window)))
    (when (and (window-live-p window)
               (+popper-child-frame-window-p window))
      window)))

(defun +popper-delete-popup-window (window)
  "Close popup WINDOW, deleting child frames when needed."
  (if (+popper-child-frame-window-p window)
      (delete-frame (window-frame window))
    (delete-window window)))

(defun +popper-close-floating-popup ()
  "Close the currently selected Popper floating popup."
  (interactive)
  (let ((window (selected-window)))
    (when (window-live-p window)
      (if (+popper-child-frame-window-p window)
          (+popper-delete-popup-window window)
        (quit-window nil window)))))

(defun +popper-child-frame-parameters ()
  "Return frame parameters for Popper floating child frames."
  `((parent-frame . ,(+popper-top-level-frame))
    (undecorated . t)
    (skip-taskbar . t)
    (no-other-frame . t)
    (unsplittable . t)
    (no-accept-focus . nil)
    (internal-border-width . 12)
    (child-frame-border-width . 1)
    (child-frame-border-color . ,+popper-posframe-border-color)
    (border-color . ,+popper-posframe-border-color)
    ;; Enable mouse dragging for the floating child frame.
    (drag-with-mode-line . t)
    (drag-with-header-line . t)
    (drag-internal-border . t)
    (left-fringe . 8)
    (right-fringe . 8)))

(defun +popper-center-child-frame (window)
  "Center the child frame of WINDOW inside its parent frame."
  (when (window-live-p window)
    (let* ((child-frame (window-frame window))
           (parent-frame (frame-parent child-frame)))
      (when (and (frame-live-p child-frame)
                 (frame-live-p parent-frame))
        (let* ((parent-width (frame-pixel-width parent-frame))
               (parent-height (frame-pixel-height parent-frame))
               (child-width (frame-pixel-width child-frame))
               (child-height (frame-pixel-height child-frame))
               (left (max 0 (/ (- parent-width child-width) 2)))
               (top (max 0 (/ (- parent-height child-height) 2))))
          (set-frame-position child-frame left top))))))

(defun +popper-center-floating-popup ()
  "Center the currently selected floating Popper popup."
  (interactive)
  (when-let* ((window (+popper-current-floating-popup-window)))
    (+popper-center-child-frame window)))

(defun +popper-move-floating-popup (dx dy)
  "Move the currently selected floating Popper popup by DX and DY pixels."
  (when-let* ((window (+popper-current-floating-popup-window))
              (child-frame (window-frame window))
              (parent-frame (frame-parent child-frame)))
    (let* ((child-width (frame-pixel-width child-frame))
           (child-height (frame-pixel-height child-frame))
           (parent-width (frame-pixel-width parent-frame))
           (parent-height (frame-pixel-height parent-frame))
           (max-left (max 0 (- parent-width child-width)))
           (max-top (max 0 (- parent-height child-height)))
           (left0 (let ((left (frame-parameter child-frame 'left)))
                    (if (numberp left) left (car (frame-position child-frame)))))
           (top0 (let ((top (frame-parameter child-frame 'top)))
                   (if (numberp top) top (cdr (frame-position child-frame)))))
           (left (min max-left (max 0 (+ left0 dx))))
           (top (min max-top (max 0 (+ top0 dy)))))
      (set-frame-position child-frame left top))))

(defun +popper-move-floating-popup-left ()
  "Move the active floating Popper popup left."
  (interactive)
  (+popper-move-floating-popup (- +popper-floating-popup-move-step) 0))

(defun +popper-move-floating-popup-right ()
  "Move the active floating Popper popup right."
  (interactive)
  (+popper-move-floating-popup +popper-floating-popup-move-step 0))

(defun +popper-move-floating-popup-up ()
  "Move the active floating Popper popup up."
  (interactive)
  (+popper-move-floating-popup 0 (- +popper-floating-popup-move-step)))

(defun +popper-move-floating-popup-down ()
  "Move the active floating Popper popup down."
  (interactive)
  (+popper-move-floating-popup 0 +popper-floating-popup-move-step))

(defun +popper-display-child-frame (buffer &optional _alist)
  "Display Popper BUFFER in a floating child frame and return its window."
  (let* ((parent-frame (+popper-top-level-frame))
         (window (with-selected-frame parent-frame
                   (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,(+popper-child-frame-parameters))
                      (window-height . fit-window-to-buffer)
                      (window-width . 0.72)
                      (dedicated . t)
                      (preserve-size . (t . t)))))))
    (when (window-live-p window)
      (set-window-dedicated-p window t)
      (with-current-buffer buffer
        (+popper-ensure-popup-modeline)
        (+popper-floating-popup-mode 1))
      (+popper-center-child-frame window)
      ;; Re-center once more after redisplay in case frame size changed.
      (run-with-idle-timer 0 nil #'+popper-center-child-frame window)
      window)))

(provide 'lib-popper)
;;; lib-popper.el ends here
