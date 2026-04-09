;;; lib-org.el --- Org helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dired-copy-images-links ()
  "Copy links of marked image files in Dired to the `kill-ring`."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let* ((marked-files (dired-get-marked-files))
             (number-marked-files (string-to-number
                                   (dired-number-of-marked-files))))
        (when (= number-marked-files 0)
          (dired-toggle-marks)
          (setq marked-files (dired-get-marked-files)))
        (message "Files marked for copy")
        (dired-number-of-marked-files)
        (kill-new "\n")
        (dolist (marked-file marked-files)
          (when (org-file-image-p marked-file)
            (kill-append
             (concat "#+CAPTION: "
                     (file-name-base marked-file)
                     "\n#+ATTR_ORG: :width 800"
                     "\n[[file:"
                     ;; Use absolute path if needed
                     (replace-regexp-in-string "^\\(~/\\|/Users/[^/]+/\\)Library/CloudStorage/Dropbox/org/[^/]*/" "" marked-file)
                     "]]\n\n")
             nil)))
        (when (= number-marked-files 0)
          (dired-toggle-marks)))
    (message "Error: Does not work outside dired-mode")))

(defun +org-emphasize-below-point (&optional char)
  "Emphasize region with CHAR.

If there's no region, marks the closest sexp first."
  (interactive)
  (unless (region-active-p)
    (backward-sexp)
    (mark-sexp))
  (org-emphasize char))

(defun +org-emphasize-bindings ()
  "Install local Org emphasis bindings."
  (dolist (binding '(("s-i b" ?*)
                     ("s-i i" ?/)
                     ("s-i u" ?_)
                     ("s-i v" ?=)
                     ("s-i c" ?~)
                     ("s-i s" ?+)))
    (let ((key (car binding))
          (char (cadr binding)))
      (define-key org-mode-map (kbd key)
                  `(lambda () (interactive) (+org-emphasize-below-point ,char))))))

(provide 'lib-org)
;;; lib-org.el ends here
