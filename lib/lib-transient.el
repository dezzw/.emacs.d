;; lib-transient.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defun browse-path (&optional args)
  "Browse files from the repositories cloned by `straight', using `fd'.
ARGS should be a list where the first element is the path to the repositories."
  (interactive (list (transient-args 'emacs-access-transient)))
  (let* ((key (car args))
         (repopath (cond
                    ((string-equal key "agenda") (concat *org-path* "/agenda/"))
                    ((string-equal key "books") (concat *org-path* "/bib/files"))
                    (t (expand-file-name key))))
         (fd-cmd (concat "fd --no-ignore-vcs . --base-directory " repopath))
         (files (cl-remove-if #'string-empty-p (split-string (shell-command-to-string fd-cmd) "\n")))
         (file (completing-read "Find file: " files nil t)))
    (find-file (file-name-concat repopath file))))

(defun +java-to-xml-mapper ()
  "Jump from a Java mapper file to the corresponding XML mapper file.
If the cursor is on a method name in the Java file, jump to the corresponding
method definition in the XML file."
  (interactive)
  (let* ((java-file (buffer-file-name))
         (xml-file (concat (file-name-sans-extension java-file) ".xml"))
         (method-name (thing-at-point 'symbol t)))
    (if (file-exists-p xml-file)
        (progn
          (find-file xml-file)
          (goto-char (point-min))
          (if (re-search-forward (concat "id=\"\\(" method-name "\\)\"") nil t)
              (message "Jumped to method: %s" method-name)
            (message "Method '%s' not found in XML file." method-name)))
      (message "No corresponding XML file found."))))

;; Git Worktree Helper Functions
(defun git-worktree-add-from-develop ()
  "Create a new git worktree from origin/develop.
Fetches latest changes, prompts for branch name, and creates a worktree
in a sibling directory with the new branch."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (branch-name (read-string "New branch name: "))
         (parent-dir (file-name-directory (directory-file-name project-root)))
         (worktree-path (expand-file-name branch-name parent-dir)))
    (when (string-empty-p branch-name)
      (user-error "Branch name cannot be empty"))
    (when (file-exists-p worktree-path)
      (user-error "Worktree path already exists: %s" worktree-path))
    ;; Fetch latest from origin
    (message "Fetching latest from origin...")
    (shell-command "git fetch origin")
    ;; Create worktree with new branch from origin/develop
    (let ((cmd (format "git worktree add %s -b %s origin/develop"
                       (shell-quote-argument worktree-path)
                       (shell-quote-argument branch-name))))
      (message "Creating worktree: %s" cmd)
      (if (= 0 (shell-command cmd))
          (progn
            (message "Successfully created worktree at: %s" worktree-path)
            (when (yes-or-no-p (format "Open worktree in new project? "))
              (project-switch-project worktree-path)))
        (error "Failed to create worktree")))))

;;;; provide
(provide 'lib-transient)
;;; lib-transient.el ends here.
