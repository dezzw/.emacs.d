;;; lib-tabspaces.el --- Consult integration for tabspaces  -*- lexical-binding: t -*-
;;; Commentary:
;;; Keep `consult-buffer' aligned with `tabspaces-mode' so the default buffer
;;; source stays workspace-local while tabspaces are active.
;;; Code:

(require 'consult)

(defvar consult--source-workspace
  (list :name     "Workspace Buffers"
        :narrow   ?w
        :history  'buffer-name-history
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda ()
                    (consult--buffer-query
                     :predicate #'tabspaces--local-buffer-p
                     :sort 'visibility
                     :as #'buffer-name)))
  "Workspace-local buffer source for `consult-buffer'.")

(defun +tabspaces-consult-sync ()
  "Sync `consult-buffer' sources with the current `tabspaces-mode' state."
  (cond
   ((and (bound-and-true-p tabspaces-mode)
         (boundp 'consult--source-buffer))
    (setq consult--source-buffer
          (plist-put (plist-put consult--source-buffer :hidden t)
                     :default nil))
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
   (t
    (when (boundp 'consult--source-buffer)
      (setq consult--source-buffer
            (plist-put (plist-put consult--source-buffer :hidden nil)
                       :default t)))
    (setq consult-buffer-sources
          (remove 'consult--source-workspace consult-buffer-sources)))))

(defun +tabspaces-consult-setup ()
  "Install README-style `consult' integration for `tabspaces'."
  (add-hook 'tabspaces-mode-hook #'+tabspaces-consult-sync)
  (+tabspaces-consult-sync))

(provide 'lib-tabspaces)
;;; lib-tabspaces.el ends here
