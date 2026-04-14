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

(defun +tabspaces-consult-setup ()
  (plist-put consult-source-buffer :hidden t)
  (plist-put consult-source-buffer :default nil)
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))


(provide 'lib-tabspaces)
;;; lib-tabspaces.el ends here
