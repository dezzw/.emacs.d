;;; package --- org-mode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package org
  :defer
  :straight `(org
              :fork (:host nil
			   :repo "https://git.tecosaur.net/tec/org-mode.git"
			   :branch "dev"
			   :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
		(require 'lisp-mnt)
		(let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
			(with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil)
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . visual-line-mode))
  :config
  (setq org-html-head-include-default-style nil
        org-adapt-indentation t
	org-use-sub-superscripts nil
	;;; begin org-modern
	org-tags-column 0
	org-auto-align-tags nil
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t
	org-pretty-entities t
	org-agenda-tags-column 0
        ;;; end org-modern
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (if (eq system-type 'darwin)
      (setq org-agenda-files '("~/Documents/Org/Planner")))
  ;; Custom TODO states and Agendas
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "TBA(b)" "|" "DONE(d!)")))

  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("review" . ?r)
          ("assignment" . ?a)
          ("lab" . ?l)
          ("test" . ?t)
          ("quiz" . ?q)
          ("pratice" . ?p)
          ("emacs" . ?e)
          ("note" . ?n)
          ("idea" . ?i))))

(use-package org-contrib
  :straight t
  :after org)

(use-package org-latex-preview
  :hook (org-mode . org-latex-preview-auto-mode)
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)
  
  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll
		    scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

(use-package org-modern
  :straight t
  :after org
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("⏵" . "⏷") ("▹" . "▿") ("▸" . "▾"))))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :after org
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package visual-fill-column
  :straight t
  :hook org-mode
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package valign
  :straight t
  :hook org-mode)

(use-package org-appear
  :straight t
  :hook org-mode)

(use-package org-tidy
  :straight t
  :hook
  (org-mode . org-tidy-mode)
  :custom
  (org-tidy-properties-style 'fringe))

(with-eval-after-load "ob"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)
     (python . t)
     (sql . t)
     (java . t)))

  (setq org-confirm-babel-evaluate nil))

(use-package org-super-agenda
  :straight t
  :hook org-agenda-mode
  :commands (org-agenda)
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-start-with-log-mode t)

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)

                        (org-super-agenda-groups
                         '((:name "Today"
				  :time-grid t
				  :date today
				  :scheduled today
				  :order 1)
                           (:name "Due Soon"
				  :deadline future
				  :order 2)
                           (:discard (:anything t))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Overdue"
				   :deadline past
				   :order 1)
                            (:name "Assignments"
				   :tag "assignment"
				   :order 2)
                            (:name "Labs"
				   :tag "lab"
				   :order 3)
                            (:name "Quizs"
				   :tag "quiz"
				   :order 4)
                            (:name "Tests/Exam"
				   :tag "test"
				   :order  5)
                            (:name "Projects"
				   :tag "Project"
				   :order 14)
                            (:name "Emacs"
				   :tag "Emacs"
				   :order 13)
                            (:discard (:anything t)))))))))))

;; Refiling
(setq org-refile-targets
      '(("~/Documents/Org/Planner/Archive.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Capture Templates
(defun dw/read-file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/Documents/Org/Planner/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

(use-package org-node
  :straight t
  :after org
  :bind
  (("C-c o f" . org-node-find)
   ("C-c o i" . org-node-insert-link))
  :custom
  (org-node-extra-id-dirs '("~/Documents/Org/Notes/"))
  :config
  (org-node-cache-mode))

(use-package org-download
  :straight t
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-image-dir "./images/"))

;; (use-package org-imagine
;;   :straight 

;; (defun org-insert-image ()
;;   "insert a image from clipboard"
;;   (interactive)
;;   (let* ((path (concat default-directory "img/"))
;; 	 (image-file (concat
;; 		      path
;; 		      (buffer-name)
;; 		      (format-time-string "_%Y%m%d_%H%M%S.png"))))
;;     (if (not (file-exists-p path))
;; 	(mkdir path))
;;     (do-applescript (concat
;; 		     "set the_path to \"" image-file "\" \n"
;; 		     "set png_data to the clipboard as «class PNGf» \n"
;; 		     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
;; 		     "write png_data to the_file \n"
;; 		     "close access the_file"))
;;     ;; (shell-command (concat "pngpaste " image-file))
;;     (org-insert-link nil
;; 		     (concat "file:" image-file)
;; 		     "")
;;     (message image-file))
;;   (org-display-inline-images))

(provide 'init-org)
;;; init-org.el ends here
