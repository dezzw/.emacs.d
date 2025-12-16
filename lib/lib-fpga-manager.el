;;; lib-fpga-manager.el --- Interactive FPGA management -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: Desmond Wang
;; Version: 2.0
;; Package-Requires: ((emacs "29.1") (transient "0.3.0"))
;; Keywords: tools, hardware
;; URL: https://github.com/your-repo/fpga-manager

;;; Commentary:

;; This package provides an interactive interface for managing FPGA locks.
;; It displays a tabulated list of FPGAs organized into three sections:
;; - MY FPGAs: FPGAs locked by you
;; - IN USE: FPGAs locked by others
;; - FREE: Available FPGAs
;;
;; Usage:
;;   M-x fpga-manager-status
;;
;; Keybindings:
;;   l - Lock FPGA at point
;;   u - Unlock FPGA at point
;;   f - Force unlock FPGA
;;   g - Refresh status
;;   t - Test menu
;;   U - Set current user
;;   ? - Show menu
;;   q - Quit

;;; Code:

(require 'tabulated-list)
(require 'project)
(require 'transient)

;;; Customization

(defgroup fpga-manager nil
  "Interactive FPGA lock management."
  :group 'tools
  :prefix "fpga-manager-")

(defcustom fpga-manager-python-command "uv run"
  "Python command to use for running the script."
  :type 'string
  :group 'fpga-manager)

(defcustom fpga-manager-current-user nil
  "Current user name for filtering \\='My FPGAs\\='.
Should be set to the exact username as it appears in the FPGA table,
e.g., \"DESMOND.WANG\" or just \"DESMOND\".
If nil, will auto-detect based on system username."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Username"))
  :group 'fpga-manager)

;;; Internal Variables

(defvar fpga-manager--entries nil
  "List of FPGA entries for the current view.")

;;; Faces

(defface fpga-manager-section-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for section headers in FPGA manager."
  :group 'fpga-manager)

(defface fpga-manager-separator
  '((t :inherit shadow))
  "Face for separators in FPGA manager."
  :group 'fpga-manager)

;;; Parsing Functions

(defun fpga-manager--parse-table-line (line)
  "Parse a single LINE from the FPGA status table.
Returns a list with env, user, date, reserved, motherboard, cards, model.
Returns nil if LINE is invalid."
  (when (string-match "^|\\s-*\\([^|]+\\)\\s-*|\\s-*\\([^|]*\\)\\s-*|\\s-*\\([^|]*\\)\\s-*|\\s-*\\([^|]*\\)\\s-*|\\s-*\\([^|]*\\)\\s-*|\\s-*\\([^|]*\\)\\s-*|\\s-*\\([^|]*\\)\\s-*|" line)
    (let ((env (string-trim (match-string 1 line)))
          (user (string-trim (match-string 2 line)))
          (date (string-trim (match-string 3 line)))
          (reserved (string-trim (match-string 4 line)))
          (motherboard (string-trim (match-string 5 line)))
          (numofcards (string-trim (match-string 6 line)))
          (model (string-trim (match-string 7 line))))
      (when (string-match "^CLI[0-9]+" env)
        (list env user date reserved motherboard numofcards model)))))

(defun fpga-manager--parse-output (output)
  "Parse the OUTPUT from linuxPC_Lock.py into a list of entries.
Each entry is (id [env user date reserved motherboard numofcards model])."
  (let ((lines (split-string output "\n"))
        (entries '()))
    (dolist (line lines)
      (when-let* ((parsed (fpga-manager--parse-table-line line)))
        (let* ((env (car parsed))
               (entry (list env (vconcat parsed))))
          (push entry entries))))
    (nreverse entries)))

;;; Entry Accessors

(defun fpga-manager--get-env-at-point ()
  "Get the ENV (CLI*) name from the current row.
Returns nil if point is on a section header."
  (let ((id (tabulated-list-get-id)))
    (when (and id (not (string-prefix-p "SECTION:" id)))
      id)))

(defun fpga-manager--get-user (entry)
  "Get the user from ENTRY."
  (when entry
    (aref (cadr entry) 1)))

(defun fpga-manager--get-reserved (entry)
  "Get the reserved field from ENTRY."
  (when entry
    (aref (cadr entry) 3)))

;;; Entry Predicates

(defun fpga-manager--is-locked-p (entry)
  "Check if ENTRY is locked (has a user)."
  (when entry
    (let ((user (fpga-manager--get-user entry)))
      (not (string-empty-p user)))))

(defun fpga-manager--is-mine-p (entry)
  "Check if ENTRY belongs to current user.
Returns t if the user field contains the current username."
  (when (and entry fpga-manager-current-user)
    (let* ((user-field (downcase (string-trim (fpga-manager--get-user entry))))
           (current-user (downcase (string-trim fpga-manager-current-user))))
      (string-match-p (regexp-quote current-user) user-field))))

;;; Section Builders

(defun fpga-manager--make-section-header (title)
  "Create a section header entry with TITLE."
  (list (concat "SECTION:" title)
        (vector title "" "" "" "" "" "")))

(defun fpga-manager--make-separator ()
  "Create a separator line entry."
  (list "SECTION:separator"
        (vector (make-string 120 ?─) "" "" "" "" "" "")))

(defun fpga-manager--add-section (result title entries)
  "Add a section with TITLE and ENTRIES to RESULT.
Returns the updated RESULT."
  (when entries
    (when result
      (push (fpga-manager--make-section-header "") result))
    (push (fpga-manager--make-separator) result)
    (push (fpga-manager--make-section-header title) result)
    (push (fpga-manager--make-separator) result)
    (dolist (entry (nreverse entries))
      (push entry result)))
  result)

(defun fpga-manager--categorize-entries (entries)
  "Categorize ENTRIES into groups: mine, free, in-use.
Returns a list with section headers and entries."
  (let ((mine '())
        (in-use '())
        (free '())
        (result '()))
    ;; Categorize entries
    (dolist (entry entries)
      (cond
       ((fpga-manager--is-mine-p entry) (push entry mine))
       ((fpga-manager--is-locked-p entry) (push entry in-use))
       (t (push entry free))))
    ;; Build result with sections in order: mine, free, in-use
    (setq result (fpga-manager--add-section result "  MY FPGAs" mine))
    (setq result (fpga-manager--add-section result "  FREE" free))
    (setq result (fpga-manager--add-section result "  IN USE" in-use))
    (nreverse result)))

;;; Script Execution

(defun fpga-manager--run-script (args)
  "Run linuxPC_Lock.py with ARGS and return output."
  (let* ((proj (project-current))
         (proj-root (if proj (project-root proj) default-directory))
         (fpga-dir (expand-file-name "fpga" proj-root))
         (default-directory fpga-dir)
         (cmd (format "%s linuxPC_Lock.py %s"
                      fpga-manager-python-command
                      args)))
    (shell-command-to-string cmd)))

(defun fpga-manager--show-command-output (cmd output)
  "Show command CMD OUTPUT in a buffer."
  (let ((buffer (get-buffer-create "*FPGA Manager Output*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Command: %s\n\n" cmd))
        (insert output)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer '((display-buffer-reuse-window
                              display-buffer-below-selected)
                             (inhibit-same-window . t)))))

;;; Interactive Commands - Lock/Unlock

(defun fpga-manager-lock ()
  "Lock the FPGA at point."
  (interactive)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let ((entry (assoc env fpga-manager--entries))
            (status-buffer (current-buffer)))
        (if (fpga-manager--is-locked-p entry)
            (message "%s is already locked" env)
          (let ((output (fpga-manager--run-script (format "-p %s -l 1" env))))
            (fpga-manager--show-command-output (format "Lock %s" env) output)
            (sit-for 1)
            (with-current-buffer status-buffer
              (fpga-manager-refresh)))))
    (message "No ENV found at point")))

(defun fpga-manager-unlock ()
  "Unlock the FPGA at point."
  (interactive)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let ((entry (assoc env fpga-manager--entries))
            (status-buffer (current-buffer)))
        (if (not (fpga-manager--is-locked-p entry))
            (message "%s is not locked" env)
          (let ((output (fpga-manager--run-script (format "-p %s -l 0" env))))
            (fpga-manager--show-command-output (format "Unlock %s" env) output)
            (sit-for 1)
            (with-current-buffer status-buffer
              (fpga-manager-refresh)))))
    (message "No ENV found at point")))

(defun fpga-manager-force-unlock ()
  "Force unlock the FPGA at point."
  (interactive)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let ((status-buffer (current-buffer)))
        (when (yes-or-no-p (format "Force unlock %s? " env))
          (let ((output (fpga-manager--run-script (format "-p %s -l 2" env))))
            (fpga-manager--show-command-output (format "Force unlock %s" env) output)
            (sit-for 1)
            (with-current-buffer status-buffer
              (fpga-manager-refresh)))))
    (message "No ENV found at point")))

;;; Interactive Commands - View

(defun fpga-manager-refresh ()
  "Refresh the FPGA lock table."
  (interactive)
  (message "Fetching FPGA status...")
  (let* ((output (fpga-manager--run-script ""))
         (entries (fpga-manager--parse-output output))
         (categorized (fpga-manager--categorize-entries entries)))
    (setq fpga-manager--entries entries)
    (setq tabulated-list-entries categorized)
    (tabulated-list-print t)
    (message "Refreshed.")))

(defun fpga-manager-set-user ()
  "Set the current user for \\='My FPGAs\\=' filtering."
  (interactive)
  (let ((user (read-string "Enter your username (as shown in FPGA table): " 
                           fpga-manager-current-user)))
    (setq fpga-manager-current-user (string-trim user))
    (message "Current user set to: %s" fpga-manager-current-user)
    (fpga-manager-refresh)))

;;; Transient Menus

(transient-define-prefix fpga-manager-menu ()
  "Main transient menu for FPGA manager operations."
  ["FPGA Manager"
   ["Lock Operations"
    ("l" "Lock FPGA at point" fpga-manager-lock)
    ("u" "Unlock FPGA at point" fpga-manager-unlock)
    ("f" "Force unlock FPGA" fpga-manager-force-unlock)]
   ["View & Settings"
    ("g" "Refresh status" fpga-manager-refresh)
    ("U" "Set current user" fpga-manager-set-user)
    ("q" "Quit window" quit-window)]
   ["Testing"
    ("t" "Test menu" fpga-manager-test-menu)]])

(defun fpga-manager-help ()
  "Show help menu for fpga-manager-mode."
  (interactive)
  (fpga-manager-menu))

;;; Test Functions

(defun fpga-manager-find-toplevel-script ()
  "Find toplevel.py script in current directory tree."
  (if-let* ((search-path (locate-dominating-file default-directory "bitstreams"))
            (script-path (expand-file-name "bitstreams/toplevel.py" search-path))
            ((file-exists-p script-path)))
      script-path
    (error "Could not find bitstreams/toplevel.py in directory tree")))

(defun fpga-manager-run-toplevel (&optional args)
  "Run toplevel.py with ARGS for the CLI at point."
  (interactive (list (transient-args 'fpga-manager-test-menu)))
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let* ((status-buffer (current-buffer))
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
        (message "Running test: %s" cmd)
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Running: %s\n" cmd))
            (insert (format "In directory: %s\n\n" default-directory))
            (insert "Test started...\n")
            (special-mode)))
        (display-buffer output-buffer '((display-buffer-reuse-window
                                         display-buffer-below-selected)
                                        (inhibit-same-window . t)))
        (select-window status-window)
        (async-shell-command cmd output-buffer)
        (run-with-timer 2 nil
                        (lambda ()
                          (when (buffer-live-p status-buffer)
                            (with-current-buffer status-buffer
                              (fpga-manager-refresh))))))
    (message "No ENV found at point")))

(transient-define-prefix fpga-manager-test-menu ()
  "Transient menu for running FPGA tests."
  ["FPGA Test Options"
   ["Switches"
    ("-d" "Debug mode (skip Build/Burn/Power)" "-d")
    ("-a" "Admin mode (force restricted CLI)" "-a")]
   ["Arguments"
    ("-r" "Repeat count" "-r " :class transient-option :prompt "Repeat: ")
    ("-l" "Log level" "-l " :class transient-option
     :choices ("20" "50") :prompt "Level: ")
    ("-c" "Custom args" "-c " :class transient-option
     :prompt "Custom (key:value): ")
    ("-L" "Local tags" "--local " :class transient-option :prompt "Tags: ")]]
  ["Actions"
   ("RET" "Run test" fpga-manager-run-toplevel)
   ("r" "Run test" fpga-manager-run-toplevel)
   ("q" "Quit" transient-quit-one)])

;;; Major Mode

(defun fpga-manager--detect-current-user ()
  "Detect current user from $USER environment variable."
  (unless fpga-manager-current-user
    (setq fpga-manager-current-user 
          (or (getenv "USER") (user-login-name)))))

(defun fpga-manager--print-entry (id cols)
  "Print a single entry with ID and COLS.
Section headers and separators are styled differently."
  (if (string-prefix-p "SECTION:" id)
      (let ((title (aref cols 0)))
        (cond
         ((string-match-p "^─+$" title)
          (insert (propertize title 'face 'fpga-manager-separator))
          (insert "\n"))
         ((not (string-empty-p title))
          (insert (propertize title 'face 'fpga-manager-section-header))
          (insert "\n"))))
    (tabulated-list-print-entry id cols)))

(defvar fpga-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "l") #'fpga-manager-lock)
    (define-key map (kbd "u") #'fpga-manager-unlock)
    (define-key map (kbd "f") #'fpga-manager-force-unlock)
    (define-key map (kbd "g") #'fpga-manager-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'fpga-manager-lock)
    (define-key map (kbd "?") #'fpga-manager-menu)
    (define-key map (kbd "t") #'fpga-manager-test-menu)
    (define-key map (kbd "U") #'fpga-manager-set-user)
    map)
  "Keymap for `fpga-manager-mode'.")

(define-derived-mode fpga-manager-mode tabulated-list-mode "FPGA-Manager"
  "Major mode for managing FPGA locks interactively.

\\{fpga-manager-mode-map}"
  (setq tabulated-list-format
        [("Env" 8 t)
         ("User" 26 t)
         ("Date" 16 t)
         ("Reserved" 30 t)
         ("Motherboard" 22 t)
         ("Cards" 6 t)
         ("Model" 8 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local tabulated-list-printer #'fpga-manager--print-entry)
  (add-hook 'tabulated-list-revert-hook #'fpga-manager-refresh nil t)
  (tabulated-list-init-header))

;;; Entry Point

;;;###autoload
(defun fpga-manager-status ()
  "Display FPGA Manager status in an interactive buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*FPGA Manager Status*")))
    (with-current-buffer buffer
      (fpga-manager-mode)
      (unless fpga-manager-current-user
        (fpga-manager--detect-current-user))
      (fpga-manager-refresh))
    (switch-to-buffer buffer)
    (message "Press '?' for help")))

(provide 'lib-fpga-manager)
;;; lib-fpga-manager.el ends here
