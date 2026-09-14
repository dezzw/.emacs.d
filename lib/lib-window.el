;;; lib-window.el --- window setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun toggle-window-hard-dedicated ()
  "Toggle strong dedication and protection from `delete-other-windows'."
  (interactive)
  (let* ((win (selected-window))
         (on (and (eq (window-dedicated-p win) t)
                  (window-parameter win 'no-delete-other-windows))))
    (if on
        (progn
          (set-window-dedicated-p win nil)
          (set-window-parameter win 'no-delete-other-windows nil)
          (message "Window is no longer strongly dedicated/protected"))
      (set-window-dedicated-p win t)
      (set-window-parameter win 'no-delete-other-windows t)
      (message "Window is strongly dedicated and protected from C-x 1"))))

(provide 'lib-window)
;;; lib-window.el ends here
