;;; lib-posframe.el --- Posframe helpers for floating popups -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'posframe)

(defcustom +popper-posframe-border-color "#7f849c"
  "Border color used by floating Popper posframes."
  :type 'string)

(defvar-local +popper-floating-popup-mode nil
  "Non-nil when the current buffer is displayed as a floating Popper posframe.")

(defvar +popper-floating-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'+popper-close-floating-popup)
    (define-key map (kbd "Q") #'+popper-close-floating-popup)
    (define-key map [remap quit-window] #'+popper-close-floating-popup)
    map)
  "Keymap used in floating Popper posframe buffers.")

(define-minor-mode +popper-floating-popup-mode
  "Minor mode enabled in Popper floating posframe buffers."
  :lighter nil
  :keymap +popper-floating-popup-mode-map)

(defun +popper-posframe-parent-frame (&optional window)
  "Return the top-level parent frame for WINDOW or the selected frame."
  (let ((frame (window-frame (or window (selected-window)))))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defun +popper-child-frame-window-p (window)
  "Return non-nil when WINDOW belongs to a child frame."
  (frame-parent (window-frame window)))

(defun +popper-posframe-window-p (window)
  "Return non-nil when WINDOW belongs to a posframe."
  (frame-parameter (window-frame window) 'posframe-buffer))

(defun +popper-delete-popup-window (window)
  "Close popup WINDOW, deleting its frame when needed."
  (if (+popper-posframe-window-p window)
      (posframe-delete-frame (window-buffer window))
    (if (+popper-child-frame-window-p window)
        (delete-frame (window-frame window))
      (delete-window window))))

(defun +popper-close-floating-popup ()
  "Close the currently selected Popper floating popup."
  (interactive)
  (let ((window (selected-window)))
    (when (window-live-p window)
      (+popper-delete-popup-window window))))

(defun +popper-posframe-hidehandler (_info)
  "Keep Popper-managed posframes visible until Popper closes them."
  nil)

(defun +popper-delete-posframe-popup-advice (orig-fn win)
  "Delete Popper popup WIN with posframe-aware cleanup."
  (if (+popper-posframe-window-p win)
      (+popper-delete-popup-window win)
    (funcall orig-fn win)))

(defun +popper-display-posframe (buffer &optional _alist)
  "Display Popper BUFFER in a floating posframe and return its window."
  (let* ((parent-frame (+popper-posframe-parent-frame))
         (frame (with-selected-frame parent-frame
                  (posframe-show
                   buffer
                   :parent-frame parent-frame
                   :poshandler #'posframe-poshandler-frame-center
                   :width 120
                   :height 24
                   :border-width 1
                   :border-color +popper-posframe-border-color
                   :internal-border-width 12
                   :left-fringe 8
                   :right-fringe 8
                   :respect-mode-line nil
                   :respect-header-line nil
                   :accept-focus t
                   :hidehandler #'+popper-posframe-hidehandler
                   :override-parameters
                   '((undecorated . t)
                     (skip-taskbar . t)
                     (no-other-frame . t)
                     (unsplittable . t)))))
         (window (and (frame-live-p frame)
                      (frame-root-window frame))))
    (when (window-live-p window)
      (set-window-dedicated-p window t)
      (with-current-buffer buffer
        (+popper-floating-popup-mode 1))
      window)))

(with-eval-after-load 'popper
  (advice-add 'popper--delete-popup :around #'+popper-delete-posframe-popup-advice))

(provide 'lib-posframe)
;;; lib-posframe.el ends here
