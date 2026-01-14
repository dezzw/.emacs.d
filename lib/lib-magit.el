;;; lib-magit.el --- vc setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst gptel-commit-prompt-base
  "The user provides the result of running `git diff --cached`.

Task:
- Generate a commit subject line that is short, clear, and accurately summarizes the staged changes.

Output constraints (MUST follow exactly):
- Output EXACTLY ONE line.
- No surrounding quotes.
- No markdown/code fences.
- No additional commentary, blank lines, body, footers, trailers, or lists.

Style guidelines:
- Keep it brief (aim ~50 chars for the description).
- Prefer specific nouns/verbs; avoid filler like \"update\", \"changes\", \"stuff\".
- Mention the most important area impacted (file/feature/module) when helpful.
- If multiple changes exist, summarize the primary one.

Now produce the single-line commit message."
  "Base prompt shared by `gptel-commit-prompt`.")

(defun gptel-commit-prompt ()
  "Return the system prompt used by `gptel-commit`.

If (and only if) the current git branch name starts with \=`BSTT\=, the
model must prefix the commit subject with \=`<branch_name> :\=`.

For all other branches, the commit subject should NOT be prefixed with the
branch name."
  (let* ((branch (or (ignore-errors (magit-get-current-branch)) ""))
         (bstt-branch-p (string-prefix-p "BSTT" branch))
         (format-block
          (if bstt-branch-p
              (format "Required format:\n\n%s : <[change_type]> <description>\n\nWhere:\n- <branch_name> is the current git branch name (%s).\n- <change_type> is one of: fix, add, remove, feat, refactor (choose the best fit).\n- <description> is a concise, imperative summary describing what changed.\n\nExample:\n- %s : [fix] validate refresh token\n"
                      branch branch branch)
            "Required format:\n\n<[change_type]> <description>\n\nWhere:\n- <change_type> is one of: fix, add, remove, feat, refactor (choose the best fit).\n- <description> is a concise, imperative summary describing what changed.\n\nExample:\n- [fix] validate refresh token\n")))
    (string-join
     (list gptel-commit-prompt-base
           format-block)
     "\n\n")))

(defun gptel-commit ()
  "Generate commit message with gptel and insert it into the buffer."
  (interactive)
  (require 'gptel)
  (when buffer-read-only
    (read-only-mode -1))
  (let* ((lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n")))
    (gptel-request changes :system (gptel-commit-prompt))))

(defun +magit-gptel-commit-when-ready ()
  "Auto-run `gptel-commit' when entering a Magit commit buffer.

This is intentionally deferred and guarded so it only runs when `gptel'
(and our `gptel-commit' helper) are available, similar to how
`copilot-chat-insert-commit-message-when-ready' defers work until the
implementation is loaded and the commit buffer is ready."
  (let ((commit-buffer (current-buffer)))
    (run-at-time
     0.3 nil
     (lambda ()
       (when (buffer-live-p commit-buffer)
         (with-current-buffer commit-buffer
           (gptel-commit)))))))

(defun +magit-or-vc-log-file (&optional prompt)
  "Show the version control log for the current file.

If the current file is under Git version control, use Magit's log view.
If PROMPT is provided, display the Magit log popup for additional options.
Otherwise, display the log directly.  If the file is not under Git, use
the built-in VC log view instead."
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

;; https://github.com/magit/magit/issues/3402
(defun magit-log-dangling ()
  (interactive)
  (magit-log-setup-buffer
   (-filter
    (lambda (x) (not (or (equal "" x) (s-match "error" x))))
    (s-lines
     (shell-command-to-string
      "git fsck --no-reflogs | awk '/dangling commit/ {print $3}'")))
   '("--no-walk" "--color" "--decorate" "--follow")'
   nil))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-log "s" '("d" "dangling" magit-log-dangling))
  (add-hook 'git-commit-setup-hook #'+magit-gptel-commit-when-ready))

(defun magit-fullscreen (orig-fun &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun magit-restore-screen (&rest args)
  (jump-to-register :magit-fullscreen))

(defun kill-all-blob-next-after-quit (orig-fun &rest args)
  "Kill next last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (kill-buffer prev-buffer)
    (unless magit-buffer-file-name
      (user-error "magit timemachine: You have reached the end of time"))))

(defun kill-all-blob-previous-after-quit (orig-fun &rest args)
  "Kill previous last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (unless (equal magit-buffer-file-name (buffer-file-name prev-buffer))
      (kill-buffer prev-buffer))))

(defun +magit-blob-save()
  (interactive)
  (let ((file magit-buffer-file-name)
        (blob-buf (current-buffer)))
    (when file
      (with-current-buffer (find-file file)
        (widen)
        (replace-buffer-contents  blob-buf))
      (message "save blob to file %s" file))
    (dolist (buf (buffer-list))         ;关闭此文件所有版本的blob buffer
      (with-current-buffer buf
        (when (equal magit-buffer-file-name file)
          (kill-this-buffer))))))

(defun +magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.

Convert these forms:
- First Last  -> First L
- First.Last  -> First L
- Last, First -> First L
- First       -> First (no change)

It is assumed that the author has only one or two names."
  ;; ARGS               -> '((REV AUTHOR DATE))
  ;; (car ARGS)         -> '(REV AUTHOR DATE)
  ;; (nth 1 (car ARGS)) -> AUTHOR
  (let* ((author (nth 1 (car args)))
         (author-abbr
          (cond
           ;; Last, First -> First L
           ((string-match-p "," author)
            (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author))
           ;; First.Last or First Last -> First L
           ((string-match-p "[. ]" author)
            (replace-regexp-in-string "\\`\\([^ .]+\\)\\(?:[. ]+\\)\\(.\\).*\\'" "\\1 \\2" author))
           ;; Single name -> unchanged
           (t author))))
    (setf (nth 1 (car args)) author-abbr))
  (car args))

;; replace this function with forge-browse
;; (defun +git-link-interactive ()
;;   "Open the Git repository homepage interactively.

;; This function sets the default directory to the root of the current project
;; and then prompts the user to generate a URL for the project's repository
;; using `git-link-homepage`, which is opened in the user's web browser."
;;   (interactive)
;;   (let ((default-directory (project-root (project-current t))))
;;     (browse-url (call-interactively #'git-link-homepage))))
(provide 'lib-magit)
;;; lib-magit.el ends here
