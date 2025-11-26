;;; lib-fpga-manager.el --- Interactive FPGA management -*- lexical-binding: t; -*-

;; Description: Interactive interface for managing FPGA locks
;; Author: Your Name
;; Version: 1.0

;;; Commentary:
;; Provides an interactive table view for FPGA lock management.
;; Navigate the table and perform lock/unlock operations with keybindings.

;;; Code:

(require 'tabulated-list)
(require 'project)
(require 'transient)

(defvar fpga-manager-python-command "uv run"
  "Python command to use for running the script.")

(defun fpga-manager-get-script-path ()
  "Get the path to linuxPC_Lock.py relative to project root."
  (let* ((proj (project-current))
         (root (if proj (project-root proj) default-directory)))
    (expand-file-name "fpga/linuxPC_Lock.py" root)))

(defvar fpga-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "l") 'fpga-manager-lock)
    (define-key map (kbd "u") 'fpga-manager-unlock)
    (define-key map (kbd "f") 'fpga-manager-force-unlock)
    (define-key map (kbd "g") 'fpga-manager-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "RET") 'fpga-manager-lock)
    (define-key map (kbd "?") 'fpga-manager-help)
    (define-key map (kbd "t") 'fpga-manager-test-menu)
    map)
  "Keymap for `fpga-manager-mode'.")

(defun fpga-manager-get-env-at-point ()
  "Get the ENV (CLI*) name from the current row."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[|+]?\\s-*\\(CLI[0-9]+\\)")
      (match-string 1))))

(defun fpga-manager-run-command (args &optional silent)
  "Run linuxPC_Lock.py with ARGS and display output.
If SILENT is non-nil, don't show output buffer."
  (let* ((proj (project-current))
         (proj-root (if proj (project-root proj) default-directory))
         (script-path (expand-file-name "fpga/linuxPC_Lock.py" proj-root))
         (default-directory proj-root)
         (cmd (format "%s %s %s"
                      fpga-manager-python-command
                      script-path
                      args))
         (output-buffer (get-buffer-create "*FPGA Manager Output*"))
         (status-window (selected-window)))
    (message "Running: %s" cmd)
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Running: %s\n" cmd))
        (insert (format "In directory: %s\n\n" default-directory))
        (insert (shell-command-to-string cmd))
        (goto-char (point-min))
        (special-mode)))
    (unless silent
      (display-buffer output-buffer '((display-buffer-reuse-window display-buffer-below-selected)
                                       (inhibit-same-window . t))))
    ;; Keep focus on the status window
    (select-window status-window)
    output-buffer))

(defun fpga-manager-lock ()
  "Lock the FPGA at point."
  (interactive)
  (let ((env (fpga-manager-get-env-at-point))
        (status-buffer (current-buffer)))
    (if env
        (progn
          (fpga-manager-run-command (format "-p %s -l 1" env))
          (sit-for 1)
          (with-current-buffer status-buffer
            (fpga-manager-refresh)))
      (message "No ENV found at point"))))

(defun fpga-manager-unlock ()
  "Unlock the FPGA at point."
  (interactive)
  (let ((env (fpga-manager-get-env-at-point))
        (status-buffer (current-buffer)))
    (if env
        (progn
          (fpga-manager-run-command (format "-p %s -l 0" env))
          (sit-for 1)
          (with-current-buffer status-buffer
            (fpga-manager-refresh)))
      (message "No ENV found at point"))))

(defun fpga-manager-force-unlock ()
  "Force unlock the FPGA at point."
  (interactive)
  (let ((env (fpga-manager-get-env-at-point))
        (status-buffer (current-buffer)))
    (if env
        (when (yes-or-no-p (format "Force unlock %s? " env))
          (fpga-manager-run-command (format "-p %s -l 2" env))
          (sit-for 1)
          (with-current-buffer status-buffer
            (fpga-manager-refresh)))
      (message "No ENV found at point"))))

(defun fpga-manager-refresh ()
  "Refresh the FPGA lock table."
  (interactive)
  (let* ((current-line (line-number-at-pos))
         (inhibit-read-only t)
         (proj (project-current))
         (proj-root (if proj (project-root proj) default-directory))
         (script-path (expand-file-name "fpga/linuxPC_Lock.py" proj-root))
         (default-directory proj-root))
    (erase-buffer)
    (message "Fetching FPGA status...")
    (insert (shell-command-to-string
             (format "%s %s" fpga-manager-python-command script-path)))
    (goto-char (point-min))
    (forward-line (1- current-line))
    (message "Refreshed.")))

(defun fpga-manager-help ()
  "Show help for fpga-manager-mode."
  (interactive)
  (message "l=lock, u=unlock, f=force-unlock, t=test-menu, g=refresh, q=quit, RET=lock, ?=help"))

;;; Toplevel.py test functions

(defun fpga-manager-find-toplevel-script ()
  "Find toplevel.py script in current directory tree."
  (let* ((script-name "toplevel.py")
         (search-path (locate-dominating-file default-directory "bitstreams"))
         (script-path (when search-path
                        (expand-file-name "bitstreams/toplevel.py" search-path))))
    (if (and script-path (file-exists-p script-path))
        script-path
      (error "Could not find bitstreams/toplevel.py in directory tree"))))

(defun fpga-manager-run-toplevel (&optional args)
  "Run toplevel.py with ARGS for the CLI at point."
  (interactive
   (list (transient-args 'fpga-manager-test-menu)))
  (let* ((env (fpga-manager-get-env-at-point))
         (status-buffer (current-buffer))
         (script-path (fpga-manager-find-toplevel-script))
         (default-directory (file-name-directory script-path))
         (args-string (if (listp args) (string-join args " ") args))
         (cmd (format "%s %s -p %s %s"
                      fpga-manager-python-command
                      (file-name-nondirectory script-path)
                      env
                      args-string))
         (output-buffer (get-buffer-create "*FPGA Test Output*"))
         (status-window (selected-window)))
    (if env
        (progn
          (message "Running test: %s" cmd)
          (with-current-buffer output-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Running: %s\n" cmd))
              (insert (format "In directory: %s\n\n" default-directory))
              (insert "Test started...\n")
              (special-mode)))
          (display-buffer output-buffer '((display-buffer-reuse-window display-buffer-below-selected)
                                          (inhibit-same-window . t)))
          (select-window status-window)
          ;; Run async
          (async-shell-command cmd output-buffer)
          ;; Refresh status after a short delay
          (run-with-timer 2 nil
                          (lambda ()
                            (when (buffer-live-p status-buffer)
                              (with-current-buffer status-buffer
                                (fpga-manager-refresh))))))
      (message "No ENV found at point"))))

;;; Transient menu

(transient-define-prefix fpga-manager-test-menu ()
  "Transient menu for running FPGA tests."
  ["FPGA Test Options"
   ["Switches"
    ("-d" "Debug mode (skip Build/Burn/Power)" "-d")
    ("-a" "Admin mode (force restricted CLI)" "-a")]
   ["Arguments"
    ("-r" "Repeat count" "-r " :class transient-option :prompt "Repeat: ")
    ("-l" "Log level" "-l " :class transient-option :choices ("20" "50") :prompt "Level: ")
    ("-c" "Custom args" "-c " :class transient-option :prompt "Custom (key:value): ")
    ("-L" "Local tags" "--local " :class transient-option :prompt "Tags: ")]]
  ["Actions"
   ("RET" "Run test" fpga-manager-run-toplevel)
   ("r" "Run test" fpga-manager-run-toplevel)
   ("q" "Quit" transient-quit-one)])

(defvar-local fpga-manager--highlight-overlay nil
  "Overlay for highlighting the current row content.")

(defun fpga-manager--highlight-row ()
  "Highlight only the content of the current row, excluding table dividers."
  (when fpga-manager--highlight-overlay
    (delete-overlay fpga-manager--highlight-overlay))
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[|+]?\\s-*\\(CLI[0-9]+\\)")
      ;; Find the start and end of the row content (between | characters)
      (let* ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (content-start (save-excursion
                              (beginning-of-line)
                              (when (re-search-forward "^[|]\\s-*" line-end t)
                                (match-beginning 0))))
             (content-end (save-excursion
                            (end-of-line)
                            (when (re-search-backward "\\s-*[|]$" line-start t)
                              (match-end 0)))))
        (when (and content-start content-end)
          (setq fpga-manager--highlight-overlay
                (make-overlay content-start content-end))
          (overlay-put fpga-manager--highlight-overlay
                       'face '(:background "#3a3a3a" :extend nil)))))))

(defun fpga-manager--update-highlight ()
  "Update row highlight after cursor movement."
  (fpga-manager--highlight-row))

(define-derived-mode fpga-manager-mode special-mode "FPGA-Manager"
  "Major mode for managing FPGA locks interactively.

\\{fpga-manager-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (add-hook 'post-command-hook #'fpga-manager--update-highlight nil t)
  (fpga-manager--highlight-row))

;;;###autoload
(defun fpga-manager-status ()
  "Display FPGA Manager status in an interactive buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*FPGA Manager Status*"))
         (proj (project-current))
         (proj-root (if proj (project-root proj) default-directory))
         (script-path (expand-file-name "fpga/linuxPC_Lock.py" proj-root))
         (default-directory proj-root))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Fetching FPGA status...\n")
        (insert (shell-command-to-string
                 (format "%s %s" fpga-manager-python-command script-path))))
      (fpga-manager-mode)
      (goto-char (point-min))
      ;; Skip to first CLI row
      (when (re-search-forward "^[|+]?\\s-*CLI" nil t)
        (beginning-of-line)))
    (switch-to-buffer buffer)
    (message "Press '?' for help")))

(provide 'lib-fpga-manager)
;;; lib-fpga-manager.el ends here
