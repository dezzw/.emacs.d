;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup nov
  (:match-file "\\.epub\\'")
  (:when-loaded
    (:hooks nov-mode-hook +nov-annotate-font-lock)
    (defface +nov-annotate-face
      '((t (:foreground "#86C166")))
      "Face for # in nov-annotate-face."
      :group 'nov-annotate-face)

    (defun +nov-annotate-font-lock ()
      "Set up font-lock for # in +nov-annotate-face."
      (font-lock-add-keywords
       nil
       '(("『\\(\\(?:.\\|\n\\)*?\\)』" . '+nov-annotate-face)))
      (font-lock-flush))))

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
    (keymap-global-set "C-c i m" 'org-remark-mark)
    (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))))

(setup org-remark-nov
  (:load-after nov)
  (:when-loaded (org-remark-nov-mode +1)))

(setup elfeed
  (:global "C-x w" elfeed)
  (:when-loaded
    (:also-load lib-elfeed)
    (:option elfeed-feeds +elfeed-feeds
             elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)
    (:with-map elfeed-show-mode-map
      (:bind "N" +menu-dwim--org-capture-elfeed-show
             "o" +open-link-with-mpv))
    (:with-map elfeed-search-mode-map (:bind "L" +elfeed-overview))))

(setup markdown-mode
  (:option markdown-command "pandoc --standalone --css=GTD.css"))

(setup md
  (:defer (:require md))
  (:when-loaded
    (:require md-ts-mode)
    (:with-mode md-ts-mode (:hook md-toc-mode))))

(provide 'init-reader)
;;; init-reader.el ends here
