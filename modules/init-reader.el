;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; pdf-view-themed-minor
;; Synchronize color filter with the present Emacs theme.
(setup pdf-view
  (:defer (:require pdf-tools)
          (:match-file "\\.PDF\\'"))
  (:when-loaded
    (:with-mode pdf-view-mode
      (:hook pdf-view-themed-minor-mode))))

(setup org-remark
  (:load-after org)
  (:when-loaded
    (:global-bind "C-c i m" 'org-remark-mark)
    (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))))

(setup markdown-mode
  (:option markdown-command "pandoc --standalone --css=GTD.css"))

(provide 'init-reader)
;;; init-reader.el ends here
