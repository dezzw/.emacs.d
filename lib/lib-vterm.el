;;; lib-vterm.el --- Vterm helper functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'vterm)
(require 'project)

(defun vterm-send-C-k-and-kill ()
  "Send `C-k' to libvterm, and put content in kill-ring."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

(defun project-vterm ()
  "Create or switch to a vterm buffer for the current project.
Similar to `project-eshell', each project gets its own vterm buffer."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer default-project-vterm-name)))
    (unless vterm-buffer
      (setq vterm-buffer (generate-new-buffer default-project-vterm-name))
      (with-current-buffer vterm-buffer
        (vterm-mode)))
    (pop-to-buffer vterm-buffer)))

(provide 'lib-vterm)
;;; lib-vterm.el ends here
