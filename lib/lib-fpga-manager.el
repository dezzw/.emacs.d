;;; lib-fpga-manager.el --- Interactive FPGA management -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025
;; Author: Desmond Wang
;; Version: 2.1
;; Package-Requires: ((emacs "29.1"))
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

(defvar fpga-manager-test-type 'bitstream
  "Current test type: \\='bitstream or \\='webapp.")

(defvar fpga-manager-test-last-args nil
  "Last arguments used in test menu for persistence.")

;;; History Variables

(defvar fpga-manager-test-repeat-history nil
  "History for repeat count argument.")

(defvar fpga-manager-test-log-level-history nil
  "History for log level argument.")

(defvar fpga-manager-test-custom-args-history nil
  "History for custom arguments.")

(defvar fpga-manager-test-id-history nil
  "History for test ID argument.")

(defvar fpga-manager-webapp-protocol-history '("http")
  "History for webapp protocol argument.")

(defvar fpga-manager-webapp-browser-history '("chrome")
  "History for webapp browser argument.")

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
  (when (string-match (concat "^|\\s-*\\([^|]+\\)\\s-*"
                              "|\\s-*\\([^|]*\\)\\s-*"
                              "|\\s-*\\([^|]*\\)\\s-*"
                              "|\\s-*\\([^|]*\\)\\s-*"
                              "|\\s-*\\([^|]*\\)\\s-*"
                              "|\\s-*\\([^|]*\\)\\s-*"
                              "|\\s-*\\([^|]*\\)\\s-*|")
                      line)
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
  (cl-loop for line in (split-string output "\n")
           for parsed = (fpga-manager--parse-table-line line)
           when parsed
           collect (list (car parsed) (vconcat parsed))))

;;; Entry Accessors

(defun fpga-manager--get-env-at-point ()
  "Get the ENV (CLI*) name from the current row.
Returns nil if point is on a section header."
  (when-let* ((id (tabulated-list-get-id)))
    (unless (string-prefix-p "SECTION:" id)
      id)))

(defun fpga-manager--entry-user (entry)
  "Get the user from ENTRY."
  (when entry
    (aref (cadr entry) 1)))

(defun fpga-manager--entry-locked-p (entry)
  "Check if ENTRY is locked (has a user)."
  (when entry
    (not (string-empty-p (fpga-manager--entry-user entry)))))

(defun fpga-manager--entry-mine-p (entry)
  "Check if ENTRY belongs to current user.
Returns t if the user field contains the current username."
  (when (and entry fpga-manager-current-user)
    (let ((user-field (downcase (string-trim (fpga-manager--entry-user entry))))
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
Returns the updated RESULT with section prepended."
  (if (null entries)
      result
    (append (list (fpga-manager--make-separator)
                  (fpga-manager--make-section-header title)
                  (fpga-manager--make-separator))
            entries
            (when result
              (list (fpga-manager--make-section-header "")))
            result)))

(defun fpga-manager--categorize-entries (entries)
  "Categorize ENTRIES into groups: mine, free, in-use.
Returns a list with section headers and entries."
  (let ((mine '())
        (in-use '())
        (free '()))
    ;; Categorize entries
    (dolist (entry entries)
      (cond
       ((fpga-manager--entry-mine-p entry) (push entry mine))
       ((fpga-manager--entry-locked-p entry) (push entry in-use))
       (t (push entry free))))
    ;; Build result with sections in order: MY FPGAs, FREE, IN USE
    ;; We build backwards (last section first) since add-section prepends
    (fpga-manager--add-section
     (fpga-manager--add-section
      (fpga-manager--add-section nil "  IN USE" (nreverse in-use))
      "  FREE" (nreverse free))
     "  MY FPGAs" (nreverse mine))))

;;; Script Execution

(defun fpga-manager--project-fpga-dir ()
  "Get the fpga directory path for the current project."
  (let* ((proj (project-current))
         (proj-root (if proj (project-root proj) default-directory)))
    (expand-file-name "fpga" proj-root)))

(defun fpga-manager--run-script-sync (args)
  "Run linuxPC_Lock.py with ARGS synchronously and return output."
  (let ((default-directory (fpga-manager--project-fpga-dir)))
    (shell-command-to-string
     (format "%s linuxPC_Lock.py %s" fpga-manager-python-command args))))

(defun fpga-manager--run-script-compile (args &optional on-success)
  "Run linuxPC_Lock.py with ARGS in compilation mode.
If ON-SUCCESS is provided, it will be called after successful compilation."
  (let ((default-directory (fpga-manager--project-fpga-dir))
        (compilation-buffer-name-function
         (lambda (_mode) "*FPGA Manager Output*")))
    (when on-success
      (letrec ((hook-fn
                (lambda (buffer result)
                  (when (equal (buffer-name buffer) "*FPGA Manager Output*")
                    (remove-hook 'compilation-finish-functions hook-fn)
                    (when (string-match "^finished" result)
                      (funcall on-success))))))
        (add-hook 'compilation-finish-functions hook-fn)))
    (compile (format "%s linuxPC_Lock.py %s" fpga-manager-python-command args))))

;;; Interactive Commands - Lock/Unlock

(defun fpga-manager--with-env-at-point (action)
  "Execute ACTION with ENV at point if available.
ACTION is a function that takes ENV as argument."
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (funcall action env)
    (message "No ENV found at point")))

(defun fpga-manager-lock ()
  "Lock the FPGA at point."
  (interactive)
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let ((entry (assoc env fpga-manager--entries))
           (status-buffer (current-buffer)))
       (if (fpga-manager--entry-locked-p entry)
           (message "%s is already locked" env)
         (message "Locking %s..." env)
         (fpga-manager--run-script-compile
          (format "-p %s -l 1" env)
          (lambda ()
            (when (buffer-live-p status-buffer)
              (with-current-buffer status-buffer
                (fpga-manager-refresh))))))))))

(defun fpga-manager-unlock ()
  "Unlock the FPGA at point."
  (interactive)
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let ((entry (assoc env fpga-manager--entries))
           (status-buffer (current-buffer)))
       (if (not (fpga-manager--entry-locked-p entry))
           (message "%s is not locked" env)
         (message "Unlocking %s..." env)
         (fpga-manager--run-script-compile
          (format "-p %s -l 0" env)
          (lambda ()
            (when (buffer-live-p status-buffer)
              (with-current-buffer status-buffer
                (fpga-manager-refresh))))))))))

(defun fpga-manager-force-unlock ()
  "Force unlock the FPGA at point."
  (interactive)
  (fpga-manager--with-env-at-point
   (lambda (env)
     (when (yes-or-no-p (format "Force unlock %s? " env))
       (let ((status-buffer (current-buffer)))
         (message "Force unlocking %s..." env)
         (fpga-manager--run-script-compile
          (format "-p %s -l 2" env)
          (lambda ()
            (when (buffer-live-p status-buffer)
              (with-current-buffer status-buffer
                (fpga-manager-refresh))))))))))

;;; Interactive Commands - View

(defun fpga-manager-refresh ()
  "Refresh the FPGA lock table."
  (interactive)
  (message "Fetching FPGA status...")
  (let* ((output (fpga-manager--run-script-sync ""))
         (entries (fpga-manager--parse-output output))
         (categorized (fpga-manager--categorize-entries entries)))
    (setq fpga-manager--entries entries
          tabulated-list-entries categorized)
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

;;; Test Helper Functions

(defun fpga-manager--find-script (subdir filename)
  "Find FILENAME in SUBDIR within the current directory tree."
  (if-let* ((search-path (locate-dominating-file default-directory subdir))
            (script-path (expand-file-name (concat subdir "/" filename) search-path)))
      (if (file-exists-p script-path)
          script-path
        (error "Could not find %s/%s in directory tree" subdir filename))
    (error "Could not find %s/%s in directory tree" subdir filename)))

(defun fpga-manager-find-toplevel-script ()
  "Find toplevel.py script in current directory tree."
  (fpga-manager--find-script "bitstreams" "toplevel.py"))

(defun fpga-manager-find-webapp-script ()
  "Find webapp/main.py script in current directory tree."
  (fpga-manager--find-script "webapp" "main.py"))

(defun fpga-manager-toggle-test-type ()
  "Toggle between bitstream and webapp test types."
  (interactive)
  (setq fpga-manager-test-type
        (if (eq fpga-manager-test-type 'bitstream) 'webapp 'bitstream))
  (message "Test type: %s" fpga-manager-test-type)
  (transient-setup 'fpga-manager-test-menu))

;;; Test Argument Parsing

(defun fpga-manager--parse-test-args (args)
  "Parse ARGS into an alist of (flag . value) pairs.
Handles both flag-only args like \\='-d\\=' and value args like \\='-r 5\\='."
  (let ((protocol nil)
        (test-ids nil)
        (browser nil)
        (repeat nil)
        (delay nil)
        (headless nil)
        (remaining '()))
    (dolist (arg (if (listp args) args (list args)))
      (cond
       ((string-prefix-p "--protocol=" arg)
        (setq protocol (substring arg 11)))
       ((string-prefix-p "--test-ids=" arg)
        (setq test-ids (substring arg 11)))
       ((string-prefix-p "--browser=" arg)
        (setq browser (substring arg 10)))
       ((string-prefix-p "-r " arg)
        (setq repeat (substring arg 3)))
       ((string-prefix-p "-i " arg)
        (setq test-ids (substring arg 3)))
       ((string= arg "-d")
        (setq delay t))
       ((string= arg "--headless")
        (setq headless t))
       (t
        (push arg remaining))))
    `((protocol . ,protocol)
      (test-ids . ,test-ids)
      (browser . ,browser)
      (repeat . ,repeat)
      (delay . ,delay)
      (headless . ,headless)
      (remaining . ,(nreverse remaining)))))

(defun fpga-manager--convert-bitstream-args (args)
  "Convert transient ARGS to bitstream format.
Converts \\='-i\\=' to \\='--local\\=' for bitstream tests."
  (mapcar (lambda (arg)
            (if (string-prefix-p "-i " arg)
                (concat "--local " (substring arg 3))
              arg))
          (if (listp args) args (list args))))

(defun fpga-manager--build-command-parts (base-parts parsed-args)
  "Build command parts list from BASE-PARTS and PARSED-ARGS alist.
Test IDs must come before --browser to avoid argument parsing issues."
  (let* ((parts base-parts)
         (delay (alist-get 'delay parsed-args))
         (repeat (alist-get 'repeat parsed-args))
         (browser (alist-get 'browser parsed-args))
         (test-ids (alist-get 'test-ids parsed-args))
         (remaining (alist-get 'remaining parsed-args)))
    ;; Add test IDs first (positional arguments must come before --browser)
    (when test-ids
      (setq parts (append parts (list test-ids))))
    (when remaining
      (setq parts (append parts remaining)))
    ;; Now add optional flags
    (when delay
      (setq parts (append parts '("-d"))))
    (when repeat
      (setq parts (append parts (list "--repeat" repeat))))
    (when browser
      (setq parts (append parts (list "--browser" browser))))
    parts))

;;; Test Execution

(defun fpga-manager-run-test (&optional args)
  "Run test (bitstream or webapp) with ARGS."
  (interactive (list (transient-args 'fpga-manager-test-menu)))
  (setq fpga-manager-test-last-args args)
  (if (eq fpga-manager-test-type 'bitstream)
      (fpga-manager-run-bitstream-test args)
    (fpga-manager-run-webapp-test args)))

(defun fpga-manager-run-bitstream-test (args)
  "Run bitstream toplevel.py test with ARGS for the CLI at point."
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let* ((status-buffer (current-buffer))
            (script-path (fpga-manager-find-toplevel-script))
            (default-directory (file-name-directory script-path))
            (converted-args (fpga-manager--convert-bitstream-args args))
            (args-string (string-join converted-args " "))
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
                             (fpga-manager-refresh)))))))))

(defun fpga-manager-run-webapp-test (args)
  "Run webapp main.py test with ARGS."
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let* ((script-path (fpga-manager-find-webapp-script))
            (default-directory (file-name-directory script-path))
            (parsed (fpga-manager--parse-test-args args))
            (protocol (alist-get 'protocol parsed))
            (headless (alist-get 'headless parsed)))
       (unless protocol
         (error "Protocol is required for webapp tests"))
       (let* ((base-parts (list fpga-manager-python-command
                                (file-name-nondirectory script-path)
                                protocol
                                env))
              (cmd-parts (fpga-manager--build-command-parts
                          base-parts
                          (cons (cons 'test-ids
                                      (or (alist-get 'test-ids parsed) "webapp"))
                                parsed)))
              (cmd (if headless
                       (concat "env -u XDG_CURRENT_DESKTOP "
                               (string-join cmd-parts " "))
                     (string-join cmd-parts " ")))
              (compilation-buffer-name-function
               (lambda (_mode) "*FPGA Test Output*")))
         (message "Running webapp test%s: %s"
                  (if headless " (headless)" "")
                  cmd)
         (compile cmd))))))

;;; Transient Test Menu

(transient-define-prefix fpga-manager-test-menu ()
  "Transient menu for running FPGA tests."
  :value (lambda ()
           (or fpga-manager-test-last-args
               '("-d" "--protocol=http")))
  [:description
   (lambda ()
     (let ((env (fpga-manager--get-env-at-point)))
       (if env
           (format "FPGA Test Menu - CLI: %s"
                   (propertize env 'face 'success))
         (format "FPGA Test Menu - %s"
                 (propertize "No CLI selected" 'face 'warning)))))
   ["Test Type"
    ("t"
     (lambda ()
       (format "Type: %s"
               (propertize (symbol-name fpga-manager-test-type) 'face 'success)))
     fpga-manager-toggle-test-type
     :transient t)]]
  [["Common Options"
    ("-r" "Repeat count" "-r "
     :class transient-option
     :prompt "Repeat: "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-test-repeat-history)
                            'fpga-manager-test-repeat-history)))
    ("-I" "Test ID" "-i "
     :class transient-option
     :prompt "Test ID: "
     :reader (lambda (prompt _initial-input history)
               (read-string prompt
                            (car fpga-manager-test-id-history)
                            'fpga-manager-test-id-history)))]
   ["Bitstream Options"
    :if (lambda () (eq fpga-manager-test-type 'bitstream))
    ("-d" "Debug/Delay mode" "-d")
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
                            'fpga-manager-test-custom-args-history)))]
   ["Webapp Options"
    :if (lambda () (eq fpga-manager-test-type 'webapp))
    ("-h" "Headless mode" "--headless")
    ("-p" "Protocol" "--protocol="
     :class transient-option
     :prompt "Protocol (http/https): "
     :choices ("http" "https")
     :reader (lambda (prompt _initial-input history)
               (completing-read prompt '("http" "https") nil nil
                                (car fpga-manager-webapp-protocol-history)
                                'fpga-manager-webapp-protocol-history)))
    ("-b" "Browser" "--browser="
     :class transient-option
     :prompt "Browser: "
     :choices ("chrome" "firefox" "safari" "edge")
     :reader (lambda (prompt _initial-input history)
               (completing-read prompt '("chrome" "firefox" "safari" "edge") nil nil
                                (car fpga-manager-webapp-browser-history)
                                'fpga-manager-webapp-browser-history)))]]
  [["Actions"
    ("RET" "Run test" fpga-manager-run-test)
    ("q" "Quit" transient-quit-one)]])

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
         ("Model" 8 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (setq-local tabulated-list-printer #'fpga-manager--print-entry)
  (add-hook 'tabulated-list-revert-hook #'fpga-manager-refresh nil t)
  (tabulated-list-init-header))

;;; Entry Point

(defun fpga-manager--buffer-name ()
  "Generate buffer name for current project's FPGA manager.
If in a project, use project name. Otherwise use default name."
  (if-let* ((proj (project-current))
            (proj-name (file-name-nondirectory
                        (directory-file-name (project-root proj)))))
      (format "*FPGA Manager: %s*" proj-name)
    "*FPGA Manager*"))

;;;###autoload
(defun fpga-manager-status ()
  "Display FPGA Manager status in an interactive buffer.
Each project gets its own FPGA manager buffer, similar to `project-eshell'."
  (interactive)
  (let* ((buffer-name (fpga-manager--buffer-name))
         (buffer (get-buffer buffer-name)))
    ;; Create or reuse existing buffer
    (unless buffer
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        (fpga-manager-mode)
        (fpga-manager--detect-current-user)))
    ;; Switch to buffer and refresh
    (switch-to-buffer buffer)
    (fpga-manager-refresh)
    (message "Press '?' for help")))

(provide 'lib-fpga-manager)
;;; lib-fpga-manager.el ends here
