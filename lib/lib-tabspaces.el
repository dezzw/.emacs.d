;;; lib-tabspaces.el --- Consult integration for tabspaces  -*- lexical-binding: t -*-
;;; Commentary:
;;; Keep `consult-buffer' aligned with `tabspaces-mode' so the default buffer
;;; source stays workspace-local while tabspaces are active.
;;; Code:

(require 'consult)

(declare-function tabspaces--current-tab-name "tabspaces")
(declare-function tabspaces--get-project-for-tab "tabspaces")
(declare-function ghostel-readonly-exit "ghostel")
(declare-function ghostel-send-string "ghostel")
(declare-function ghostel-send-key "ghostel")

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

(defun +ghostel-cd-workspace-root ()
  "Cd the current ghostel shell to the active tabspaces workspace root.
Intended for `ghostel-eval-cmds'; resolves the project via the current
tab-bar tab, not the shell's working directory."
  (if-let* ((tab (tabspaces--current-tab-name))
            (root (tabspaces--get-project-for-tab tab))
            (dir (expand-file-name root)))
      (when (and (boundp 'ghostel--term) ghostel--term)
        (when (memq ghostel--input-mode '(copy emacs))
          (ghostel-readonly-exit))
        (ghostel-send-string
         (concat "cd " (shell-quote-argument (file-local-name dir))))
        (ghostel-send-key "return"))
    (message "ghostel: tab %S has no project root" (or tab "?"))))


(provide 'lib-tabspaces)
;;; lib-tabspaces.el ends here
