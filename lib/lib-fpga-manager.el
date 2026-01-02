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

(defun fpga-manager--run-script-sync (args)
  "Run linuxPC_Lock.py with ARGS synchronously and return output."
  (let* ((proj (project-current))
         (proj-root (if proj (project-root proj) default-directory))
         (fpga-dir (expand-file-name "fpga" proj-root))
         (default-directory fpga-dir)
         (cmd (format "%s linuxPC_Lock.py %s"
                      fpga-manager-python-command
                      args)))
    (shell-command-to-string cmd)))

(defun fpga-manager--run-script-compile (args &optional on-success)
  "Run linuxPC_Lock.py with ARGS in compilation mode.
If ON-SUCCESS is provided, it will be called after successful compilation."
  (let* ((proj (project-current))
         (proj-root (if proj (project-root proj) default-directory))
         (fpga-dir (expand-file-name "fpga" proj-root))
         (default-directory fpga-dir)
         (cmd (format "%s linuxPC_Lock.py %s"
                      fpga-manager-python-command
                      args))
         (compilation-buffer-name-function
          (lambda (_mode) "*FPGA Manager Output*")))
    (when on-success
      ;; Set up a one-time hook to run on-success after compilation finishes
      (let ((hook-fn (lambda (buffer result)
                       (when (and (string-match "^finished" result)
                                  (equal (buffer-name buffer) "*FPGA Manager Output*"))
                         (funcall on-success)))))
        (add-hook 'compilation-finish-functions hook-fn)
        ;; Remove the hook after it runs once
        (run-with-timer 0.1 nil
                        (lambda ()
                          (remove-hook 'compilation-finish-functions hook-fn)))))
    (compile cmd)))

;;; Interactive Commands - Lock/Unlock

(defun fpga-manager-lock ()
  "Lock the FPGA at point."
  (interactive)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let ((entry (assoc env fpga-manager--entries))
            (status-buffer (current-buffer)))
        (if (fpga-manager--is-locked-p entry)
            (message "%s is already locked" env)
          (message "Locking %s..." env)
          (fpga-manager--run-script-compile
           (format "-p %s -l 1" env)
           (lambda ()
             (when (buffer-live-p status-buffer)
               (with-current-buffer status-buffer
                 (fpga-manager-refresh)))))))
    (message "No ENV found at point")))

(defun fpga-manager-unlock ()
  "Unlock the FPGA at point."
  (interactive)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let ((entry (assoc env fpga-manager--entries))
            (status-buffer (current-buffer)))
        (if (not (fpga-manager--is-locked-p entry))
            (message "%s is not locked" env)
          (message "Unlocking %s..." env)
          (fpga-manager--run-script-compile
           (format "-p %s -l 0" env)
           (lambda ()
             (when (buffer-live-p status-buffer)
               (with-current-buffer status-buffer
                 (fpga-manager-refresh)))))))
    (message "No ENV found at point")))

(defun fpga-manager-force-unlock ()
  "Force unlock the FPGA at point."
  (interactive)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (let ((status-buffer (current-buffer)))
        (when (yes-or-no-p (format "Force unlock %s? " env))
          (message "Force unlocking %s..." env)
          (fpga-manager--run-script-compile
           (format "-p %s -l 2" env)
           (lambda ()
             (when (buffer-live-p status-buffer)
               (with-current-buffer status-buffer
                 (fpga-manager-refresh)))))))
    (message "No ENV found at point")))

;;; Interactive Commands - View

(defun fpga-manager-refresh ()
  "Refresh the FPGA lock table."
  (interactive)
  (message "Fetching FPGA status...")
  (let* ((output (fpga-manager--run-script-sync ""))
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

(defvar fpga-manager-test-type 'bitstream
  "Current test type: 'bitstream or 'webapp.")

(defvar fpga-manager-test-repeat-history nil
  "History for repeat count argument.")

(defvar fpga-manager-test-log-level-history nil
  "History for log level argument.")

(defvar fpga-manager-test-custom-args-history nil
  "History for custom arguments.")

(defvar fpga-manager-test-local-tags-history nil
  "History for local tags argument.")

(defvar fpga-manager-webapp-protocol-history '("https")
  "History for webapp protocol argument.")

(defvar fpga-manager-webapp-fqdn-history nil
  "History for webapp FQDN argument.")

(defvar fpga-manager-webapp-browser-history '("chrome")
  "History for webapp browser argument.")

(defvar fpga-manager-webapp-test-ids-history '("webapp")
  "History for webapp test IDs argument.")

(defun fpga-manager-find-toplevel-script ()
  "Find toplevel.py script in current directory tree."
  (if-let* ((search-path (locate-dominating-file default-directory "bitstreams"))
            (script-path (expand-file-name "bitstreams/toplevel.py" search-path))
            ((file-exists-p script-path)))
      script-path
    (error "Could not find bitstreams/toplevel.py in directory tree")))

(defun fpga-manager-find-webapp-script ()
  "Find webapp/main.py script in current directory tree."
  (if-let* ((search-path (locate-dominating-file default-directory "webapp"))
            (script-path (expand-file-name "webapp/main.py" search-path))
            ((file-exists-p script-path)))
      script-path
    (error "Could not find webapp/main.py in directory tree")))

(defun fpga-manager-toggle-test-type ()
  "Toggle between bitstream and webapp test types."
  (interactive)
  (setq fpga-manager-test-type
        (if (eq fpga-manager-test-type 'bitstream) 'webapp 'bitstream))
  (message "Test type: %s" fpga-manager-test-type))

(defun fpga-manager-run-test (&optional args)
  "Run test (bitstream or webapp) with ARGS."
  (interactive (list (transient-args 'fpga-manager-test-menu)))
  (if (eq fpga-manager-test-type 'bitstream)
      (fpga-manager-run-bitstream-test args)
    (fpga-manager-run-webapp-test args)))

(defun fpga-manager-run-bitstream-test (args)
  "Run bitstream toplevel.py test with ARGS for the CLI at point."
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
             (compilation-buffer-name-function
              (lambda (_mode) "*FPGA Test Output*")))
        (message "Running bitstream test: %s" cmd)
        (compile cmd)
        (run-with-timer 2 nil
                        (lambda ()
                          (when (buffer-live-p status-buffer)
                            (with-current-buffer status-buffer
                              (fpga-manager-refresh))))))
    (message "No ENV found at point")))

(defun fpga-manager-run-webapp-test (args)
  "Run webapp main.py test with ARGS."
  (let* ((script-path (fpga-manager-find-webapp-script))
         (default-directory (file-name-directory script-path))
         ;; Parse args: extract protocol, fqdn, test-ids, and other flags
         (protocol nil)
         (fqdn nil)
         (test-ids nil)
         (browser nil)
         (repeat nil)
         (delay nil)
         (remaining-args '()))
    
    ;; Parse the args list
    (let ((args-list (if (listp args) args (list args))))
      (while args-list
        (let ((arg (car args-list)))
          (cond
           ((string-prefix-p "--protocol=" arg)
            (setq protocol (substring arg 11)))
           ((string-prefix-p "--fqdn=" arg)
            (setq fqdn (substring arg 7)))
           ((string-prefix-p "--test-ids=" arg)
            (setq test-ids (substring arg 11)))
           ((string-prefix-p "--browser=" arg)
            (setq browser (substring arg 10)))
           ((string-prefix-p "-r " arg)
            (setq repeat (substring arg 3)))
           ((string= arg "-d")
            (setq delay t))
           (t
            (push arg remaining-args))))
        (setq args-list (cdr args-list))))
    
    ;; Validate required arguments
    (unless protocol
      (error "Protocol is required for webapp tests"))
    (unless fqdn
      (error "FQDN is required for webapp tests"))
    
    ;; Build command
    (let* ((cmd-parts (list fpga-manager-python-command
                           (file-name-nondirectory script-path)
                           protocol
                           fqdn))
           (cmd-parts (if delay
                          (append cmd-parts '("-d"))
                        cmd-parts))
           (cmd-parts (if repeat
                          (append cmd-parts (list "--repeat" repeat))
                        cmd-parts))
           (cmd-parts (if browser
                          (append cmd-parts (list "--browser" browser))
                        cmd-parts))
           (cmd-parts (append cmd-parts (nreverse remaining-args)))
           (cmd-parts (append cmd-parts (list (or test-ids "webapp"))))
           (cmd (string-join cmd-parts " "))
           (compilation-buffer-name-function
            (lambda (_mode) "*FPGA Test Output*")))
      (message "Running webapp test: %s" cmd)
      (compile cmd))))

(transient-define-prefix fpga-manager-test-menu ()
  "Transient menu for running FPGA tests."
  :value (lambda () (list (format "--test-type=%s" fpga-manager-test-type)))
  ["Test Type"
   ("T" 
    (lambda () (format "Test: %s" (propertize (symbol-name fpga-manager-test-type) 'face 'success)))
    fpga-manager-toggle-test-type
    :transient t)]
  ["Common Options"
   ["Shared"
    ("-d" "Debug/Delay mode" "-d")
    ("-r" "Repeat count" "-r "
     :class transient-option
     :prompt "Repeat: "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-test-repeat-history)
                            'fpga-manager-test-repeat-history)))]]
  ["Bitstream Options"
   :if (lambda () (eq fpga-manager-test-type 'bitstream))
   [""
    ("-a" "Admin mode (force restricted CLI)" "-a")
    ("-l" "Log level" "-l "
     :class transient-option
     :prompt "Level: "
     :choices ("20" "50")
     :reader (lambda (prompt _initial-input history)
               (completing-read prompt '("20" "50") nil nil
                                (car fpga-manager-test-log-level-history)
                                'fpga-manager-test-log-level-history)))
    ("-c" "Custom args" "-c "
     :class transient-option
     :prompt "Custom (key:value): "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-test-custom-args-history)
                            'fpga-manager-test-custom-args-history)))
    ("-L" "Local tags" "--local "
     :class transient-option
     :prompt "Tags: "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-test-local-tags-history)
                            'fpga-manager-test-local-tags-history)))]]
  ["Webapp Options"
   :if (lambda () (eq fpga-manager-test-type 'webapp))
   [""
    ("-p" "Protocol" "--protocol="
     :class transient-option
     :prompt "Protocol (http/https): "
     :choices ("http" "https")
     :reader (lambda (prompt _initial-input history)
               (completing-read prompt '("http" "https") nil nil
                                (car fpga-manager-webapp-protocol-history)
                                'fpga-manager-webapp-protocol-history)))
    ("-f" "FQDN" "--fqdn="
     :class transient-option
     :prompt "FQDN: "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-webapp-fqdn-history)
                            'fpga-manager-webapp-fqdn-history)))
    ("-b" "Browser" "--browser="
     :class transient-option
     :prompt "Browser: "
     :choices ("chrome" "firefox" "safari" "edge")
     :reader (lambda (prompt _initial-input history)
               (completing-read prompt '("chrome" "firefox" "safari" "edge") nil nil
                                (car fpga-manager-webapp-browser-history)
                                'fpga-manager-webapp-browser-history)))
    ("-t" "Test IDs" "--test-ids="
     :class transient-option
     :prompt "Test IDs (space-separated or 'webapp'): "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-webapp-test-ids-history)
                            'fpga-manager-webapp-test-ids-history)))]]
  ["Actions"
   ("RET" "Run test" fpga-manager-run-test)
   ("r" "Run test" fpga-manager-run-test)
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
