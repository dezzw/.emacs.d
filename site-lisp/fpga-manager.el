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
;;
;; --- Elisp beginner notes (syntax used in this file) ---
;; (require 'feature)         ;; Load another library (like import).
;; (defgroup NAME ...)         ;; Define a customization group.
;; (defcustom NAME VALUE ...)  ;; User option (editable via M-x customize).
;; (defvar NAME VALUE ...)     ;; Global variable (not user-facing).
;; (defface NAME ...)          ;; Define a face (style for text).
;; (defun NAME (args) ...)     ;; Define a function.
;; (interactive)              ;; Allow calling a function via M-x or keybind.
;;
;; (let ((x 1) (y 2)) ...)     ;; Local variables for a block.
;; (if COND A B)               ;; If/else.
;; (when COND ...)             ;; If with no else (runs body when true).
;; (unless COND ...)           ;; If not.
;; (cond ((COND1) ...) ...)    ;; Multi-branch if.
;;
;; (if-let* ((x ...) (y ...)) ...)  ;; Bind and check non-nil.
;; (when-let* ((x ...) ...) ...)    ;; Like when + if-let*.
;;
;; (lambda (args) ...)         ;; Anonymous function.
;; #'symbol                   ;; Function symbol (like a pointer to a function).
;;
;; (list a b c)                ;; Build a list.
;; (vector a b c)              ;; Build a vector (array).
;; (setq var value)            ;; Assign.
;; (setq-local var value)      ;; Assign buffer-local value.
;;
;; Strings: "text", chars: ?a, symbols: 'symbol, quoted list: '(a b).
;; Property lists: '(:key value ...) and alists: '((key . value) ...).
;; ----------------------------------------------------------

;;; Code:

(require 'tabulated-list)
(require 'project)
(require 'transient)
(require 'subr-x)

;;; Customization

;; User-facing customization group so options show under M-x customize-group.
(defgroup fpga-manager nil
  "Interactive FPGA lock management."
  :group 'tools
  :prefix "fpga-manager-")

;; The Python entry point is invoked via shell.
;; Keep this a string instead of a list so users can include wrappers (e.g. "uv run",
;; "poetry run", or "python3 -m uv run"). We split/quote later when needed.
(defcustom fpga-manager-python-command "uv run"
  "Python command to use for running the script."
  :type 'string
  :group 'fpga-manager)

;; Username used for filtering "My FPGAs". We avoid forcing a default here so that
;; first-time usage can auto-detect via $USER or `user-login-name`.
(defcustom fpga-manager-current-user nil
  "Current user name for filtering \\='My FPGAs\\='.
Should be set to the exact username as it appears in the FPGA table,
e.g., \"DESMOND.WANG\" or just \"DESMOND\".
If nil, will auto-detect based on system username."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Username"))
  :group 'fpga-manager)

;;; Internal Variables

;; Raw entries produced by `fpga-manager--parse-output`. This is the canonical
;; data model for the current buffer and is reused for lookup at point.
(defvar fpga-manager--entries nil
  "List of FPGA entries for the current view.")

;; Controls which test menu subtree is active.
(defvar fpga-manager-test-type 'bitstream
  "Current test type: \\='bitstream or \\='webapp.")

;; Last transient arguments so the menu can reopen with previous choices.
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

;; Parse a single table row produced by linuxPC_Lock.py. The output is a
;; pipe-delimited table and we normalize whitespace with `string-trim`.
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
      ;; Only accept FPGA rows (CLI*). This filters out table separators.
      (when (string-match "^CLI[0-9]+" env)
        (list env user date reserved motherboard numofcards model)))))

;; Convert raw output to tabulated-list entries. Each entry is:
;; (id [env user date reserved motherboard cards model])
(defun fpga-manager--parse-output (output)
  "Parse the OUTPUT from linuxPC_Lock.py into a list of entries.
Each entry is (id [env user date reserved motherboard numofcards model])."
  (cl-loop for line in (split-string output "\n")
           for parsed = (fpga-manager--parse-table-line line)
           when parsed
           collect (list (car parsed) (vconcat parsed))))

;;; Entry Accessors

;; Accessor for tabulated-list rows; returns nil for section headers.
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

;; "Mine" detection is a substring match to handle usernames like
;; "DESMOND.WANG" vs "DESMOND". Downcasing makes it case-insensitive.
(defun fpga-manager--entry-mine-p (entry)
  "Check if ENTRY belongs to current user.
Returns t if the user field contains the current username."
  (when (and entry fpga-manager-current-user)
    (let ((user-field (downcase (string-trim (fpga-manager--entry-user entry))))
          (current-user (downcase (string-trim fpga-manager-current-user))))
      (string-match-p (regexp-quote current-user) user-field))))

;;; Section Builders

;; Section headers are regular tabulated-list entries with a special id.
;; The printer uses this id to draw headers instead of table rows.
(defun fpga-manager--make-section-header (title)
  "Create a section header entry with TITLE."
  (list (concat "SECTION:" title)
        (vector title "" "" "" "" "" "")))

;; A "separator" row is just a line of box-drawing chars.
(defun fpga-manager--make-separator ()
  "Create a separator line entry."
  (list "SECTION:separator"
        (vector (make-string 120 ?─) "" "" "" "" "" "")))

;; We prepend sections to keep the caller code simple. This also lets us build
;; the final list from back to front.
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
    ;; Categorize entries into three buckets based on lock state and user.
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

;; Prefer project root so python scripts resolve relative paths consistently.
(defun fpga-manager--project-root ()
  "Get the current project root or `default-directory'."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

;; The FPGA scripts live under fpga/ relative to project root.
(defun fpga-manager--project-fpga-dir ()
  "Get the fpga directory path for the current project."
  (expand-file-name "fpga" (fpga-manager--project-root)))

;; Split wrapper commands ("uv run") into a list of arguments.
(defun fpga-manager--command-parts (command)
  "Split COMMAND into shell-ready parts."
  (split-string-and-unquote command))

;; Quote each part and re-join into a shell command string.
(defun fpga-manager--build-command (parts)
  "Build a shell command string from PARTS."
  (mapconcat #'shell-quote-argument parts " "))

;; Buffer name includes project to avoid cross-project reuse.
(defun fpga-manager--compilation-buffer-name (project-root)
  "Return a compilation buffer name for PROJECT-ROOT."
  (format "*%s_compilation*"
          (file-name-nondirectory (directory-file-name project-root))))

;; Small helper to run a shell command with confirmation, routed to compile.
(defun fpga-manager--confirm-and-compile (default-dir cmd)
  "Confirm then compile CMD in DEFAULT-DIR."
  (let* ((project-root (fpga-manager--project-root))
         (compilation-buffer-name-function
          (lambda (_mode)
            (fpga-manager--compilation-buffer-name project-root))))
    (when (yes-or-no-p (format "Run command in %s: %s ?" default-dir cmd))
      (let ((default-directory default-dir))
        (compile cmd)))))

(defun fpga-manager--run-script-sync (args)
  "Run linuxPC_Lock.py with ARGS synchronously and return output."
  (let ((default-directory (fpga-manager--project-fpga-dir)))
    ;; Synchronous call is used only for status fetch (fast, small output).
    (shell-command-to-string
     (format "%s linuxPC_Lock.py %s" fpga-manager-python-command args))))

;; Asynchronous compile-based execution gives us clickable errors and output.
(defun fpga-manager--run-script-compile (args &optional on-success)
  "Run linuxPC_Lock.py with ARGS in compilation mode.
If ON-SUCCESS is provided, it will be called after successful compilation."
  (let ((default-directory (fpga-manager--project-fpga-dir))
        (compilation-buffer-name-function
         (lambda (_mode) "*FPGA Manager Output*")))
    (when on-success
      ;; Use a temporary completion hook to refresh the status buffer when the
      ;; command finishes successfully.
      (letrec ((hook-fn
                (lambda (buffer result)
                  (when (equal (buffer-name buffer) "*FPGA Manager Output*")
                    (remove-hook 'compilation-finish-functions hook-fn)
                    (when (string-match "^finished" result)
                      (funcall on-success))))))
        (add-hook 'compilation-finish-functions hook-fn)))
    (compile (format "%s linuxPC_Lock.py %s" fpga-manager-python-command args))))

;;; Interactive Commands - Lock/Unlock

;; Helper wrapper so commands can share "get env at point" error handling.
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
       ;; Guard against double-locking to avoid noisy errors.
       (if (fpga-manager--entry-locked-p entry)
           (message "%s is already locked" env)
         (message "Locking %s..." env)
         (fpga-manager--run-script-compile
          (format "-p %s -l 1" env)
          (lambda ()
            ;; Refresh the table once the lock command finishes.
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
       ;; Guard against unlocking an already-free device.
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
     ;; Force unlock is potentially destructive; ask for confirmation.
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
    ;; Re-render the tabulated list with current data.
    (tabulated-list-print t)
    (message "Refreshed.")))

(defun fpga-manager-set-user ()
  "Set the current user for \\='My FPGAs\\=' filtering."
  (interactive)
  (let ((user (read-string "Enter your username (as shown in FPGA table): "
                           fpga-manager-current-user)))
    (setq fpga-manager-current-user (string-trim user))
    (message "Current user set to: %s" fpga-manager-current-user)
    ;; Refresh so the section grouping reflects the new filter.
    (fpga-manager-refresh)))

;;; Transient Menus

;; The transient menu provides discoverability and keeps keybindings visible.
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

;; Locate helper scripts by walking up the directory tree.
;; This keeps the UI working from any subdirectory in the project.
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
  ;; Reopen the transient to reflect updated options.
  (transient-setup 'fpga-manager-test-menu))

;;; Test Argument Parsing

;; Transient returns an argument list that mixes flags and key/value forms.
;; We normalize it into an alist for easier command assembly.
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

;; Build the command parts in a stable order so downstream scripts parse
;; positional arguments correctly.
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
  ;; Save arguments so the next menu invocation can reuse them.
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
       ;; Persist args into BSTT state so quick actions reuse them.
       (fpga-manager--bstt-sync-from-bitstream env args)
       ;; Use compile so errors are navigable.
       (message "Running bitstream test: %s" cmd)
       (compile cmd)
       ;; Give the test a moment to start before refreshing status.
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
       ;; Protocol is required because it is a positional argument.
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
        ;; Persist args into BSTT state so quick actions reuse them.
        (fpga-manager--bstt-sync-from-webapp env args parsed)
         ;; Headless mode drops the desktop variable to avoid GUI dependency.
         (message "Running webapp test%s: %s"
                  (if headless " (headless)" "")
                  cmd)
         (compile cmd))))))

;;; Transient Test Menu

;; The transient menu captures common flags and exposes richer options by
;; dynamically switching between bitstream and webapp sets.
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
     :reader (lambda (prompt _initial-input _history)
               (read-string prompt
                            (car fpga-manager-test-repeat-history)
                            'fpga-manager-test-repeat-history)))
    ("-I" "Test ID" "-i "
     :class transient-option
     :prompt "Test ID: "
     :always-read t
     :reader (lambda (prompt _initial-input _history)
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
     :reader (lambda (prompt _initial-input _history)
               (completing-read prompt '("20" "50") nil nil
                                (car fpga-manager-test-log-level-history)
                                'fpga-manager-test-log-level-history)))
    ("-c" "Custom args" "-c "
     :class transient-option
     :prompt "Custom (key:value): "
     :reader (lambda (prompt _initial-input _history)
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
     :reader (lambda (prompt _initial-input _history)
               (completing-read prompt '("http" "https") nil nil
                                (car fpga-manager-webapp-protocol-history)
                                'fpga-manager-webapp-protocol-history)))
    ("-b" "Browser" "--browser="
     :class transient-option
     :prompt "Browser: "
     :choices ("chrome" "firefox" "safari" "edge")
     :reader (lambda (prompt _initial-input _history)
               (completing-read prompt '("chrome" "firefox" "safari" "edge") nil nil
                                (car fpga-manager-webapp-browser-history)
                                'fpga-manager-webapp-browser-history)))]]
  [["Actions"
    ("RET" "Run test" fpga-manager-run-test)
    ("q" "Quit" transient-quit-one)]])

;;; Major Mode

;; Auto-detect the user once per buffer, unless explicitly set.
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
          ;; Separator line.
          (insert (propertize title 'face 'fpga-manager-separator))
          (insert "\n"))
         ((not (string-empty-p title))
          ;; Section header.
          (insert (propertize title 'face 'fpga-manager-section-header))
          (insert "\n"))))
    (tabulated-list-print-entry id cols)))

(defvar fpga-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; One-keystroke actions for faster operation.
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
  ;; Define tabulated list columns and install custom printer so we can render
  ;; section headers inline with the data rows.
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

;; Buffer name follows the active project to avoid mixing multiple workspaces.
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

;;; BSTT Helper Commands

;; Instead of a pile of ad-hoc globals, keep BSTT defaults in one place.
;; This makes it easy to persist/extend, and lets us reuse the same history
;; variables used elsewhere in this package.

(defvar fpga-manager-bstt--state
  '(:lock-port "cli455"
    :lock-value ""
    :webapp-cli-code "CLI109"
    :webapp-batch-code "B000006"
    :webapp-browser "chrome"
    :webapp-repeat "1"
    :toplevel-cli-code "CLI455"
    :toplevel-local-code "B000006"
    :toplevel-config ""
    :toplevel-repeat "1")
  "Plist holding defaults/last values for BSTT helper commands.")

;; Back-compat variables (older config used direct `bstt/*` defvars).
;; Keep these in sync with `fpga-manager-bstt--state` so existing workflows
;; and external references continue to work.
(defvar bstt/lock-port (plist-get fpga-manager-bstt--state :lock-port))
(defvar bstt/lock-value (plist-get fpga-manager-bstt--state :lock-value))
(defvar bstt/webapp-cli-code (plist-get fpga-manager-bstt--state :webapp-cli-code))
(defvar bstt/webapp-batch-code (plist-get fpga-manager-bstt--state :webapp-batch-code))
(defvar bstt/webapp-browser (plist-get fpga-manager-bstt--state :webapp-browser))
(defvar bstt/webapp-repeat (plist-get fpga-manager-bstt--state :webapp-repeat))
(defvar bstt/toplevel-cli-code (plist-get fpga-manager-bstt--state :toplevel-cli-code))
(defvar bstt/toplevel-local-code (plist-get fpga-manager-bstt--state :toplevel-local-code))
(defvar bstt/toplevel-config (plist-get fpga-manager-bstt--state :toplevel-config))
(defvar bstt/toplevel-repeat (plist-get fpga-manager-bstt--state :toplevel-repeat))

;; Mapping between plist keys and legacy variable symbols.
(defconst fpga-manager-bstt--var-map
  '((:lock-port . bstt/lock-port)
    (:lock-value . bstt/lock-value)
    (:webapp-cli-code . bstt/webapp-cli-code)
    (:webapp-batch-code . bstt/webapp-batch-code)
    (:webapp-browser . bstt/webapp-browser)
    (:webapp-repeat . bstt/webapp-repeat)
    (:toplevel-cli-code . bstt/toplevel-cli-code)
    (:toplevel-local-code . bstt/toplevel-local-code)
    (:toplevel-config . bstt/toplevel-config)
    (:toplevel-repeat . bstt/toplevel-repeat)))

;; BSTT-specific histories (browser/repeat reuse fpga-manager histories).
(defvar fpga-manager-bstt-lock-port-history nil)
(defvar fpga-manager-bstt-lock-value-history nil)
(defvar fpga-manager-bstt-webapp-cli-code-history nil)
(defvar fpga-manager-bstt-webapp-batch-code-history nil)
(defvar fpga-manager-bstt-toplevel-cli-code-history nil)
(defvar fpga-manager-bstt-toplevel-local-code-history nil)
(defvar fpga-manager-bstt-toplevel-config-history nil)

(defun fpga-manager-bstt--get (key)
  "Get KEY from `fpga-manager-bstt--state'."
  (plist-get fpga-manager-bstt--state key))

(defun fpga-manager-bstt--sync-var (key value)
  "Sync VALUE into legacy variable for KEY."
  (when-let* ((var (cdr (assq key fpga-manager-bstt--var-map))))
    (set var value)))

(defun fpga-manager-bstt--sync-state-from-vars ()
  "Pull legacy `bstt/*` variables into `fpga-manager-bstt--state'."
  (dolist (pair fpga-manager-bstt--var-map)
    (let* ((key (car pair))
           (var (cdr pair)))
      (when (boundp var)
        (let ((val (symbol-value var)))
          (when val
            (setq fpga-manager-bstt--state
                  (plist-put fpga-manager-bstt--state key val))))))))

(defun fpga-manager-bstt--set (key value)
  "Set KEY to VALUE in `fpga-manager-bstt--state'."
  (setq fpga-manager-bstt--state
        (plist-put fpga-manager-bstt--state key value))
  (fpga-manager-bstt--sync-var key value)
  value)

(defun fpga-manager--bstt-build-bitstream-args ()
  "Build fpga-manager transient args from BSTT bitstream state."
  (fpga-manager-bstt--sync-state-from-vars)
  (let ((args '("-d")))
    (when-let* ((local (fpga-manager-bstt--get :toplevel-local-code)))
      (unless (string-empty-p local)
        (push (concat "-i " local) args)))
    (when-let* ((repeat (fpga-manager-bstt--get :toplevel-repeat)))
      (unless (string-empty-p repeat)
        (push (concat "-r " repeat) args)))
    (when-let* ((cfg (fpga-manager-bstt--get :toplevel-config)))
      (unless (string-empty-p cfg)
        (push (concat "-c " cfg) args)))
    (nreverse args)))

(defun fpga-manager--bstt-build-webapp-args ()
  "Build fpga-manager transient args from BSTT webapp state."
  (fpga-manager-bstt--sync-state-from-vars)
  (let ((args '("--protocol=http")))
    (when-let* ((batch (fpga-manager-bstt--get :webapp-batch-code)))
      (unless (string-empty-p batch)
        (push (concat "-i " batch) args)))
    (when-let* ((repeat (fpga-manager-bstt--get :webapp-repeat)))
      (unless (string-empty-p repeat)
        (push (concat "-r " repeat) args)))
    (when-let* ((browser (fpga-manager-bstt--get :webapp-browser)))
      (unless (string-empty-p browser)
        (push (concat "--browser=" browser) args)))
    (nreverse args)))

(defun fpga-manager--bstt-sync-test-menu (type)
  "Sync fpga-manager test menu defaults from BSTT state."
  (fpga-manager-bstt--sync-state-from-vars)
  (setq fpga-manager-test-type type
        fpga-manager-test-last-args
        (if (eq type 'webapp)
            (fpga-manager--bstt-build-webapp-args)
          (fpga-manager--bstt-build-bitstream-args))))

(defun fpga-manager--bstt-sync-from-bitstream (env args)
  "Update BSTT bitstream defaults from fpga-manager ENV and ARGS."
  (fpga-manager-bstt--set :toplevel-cli-code env)
  (dolist (arg (if (listp args) args (list args)))
    (cond
     ((string-prefix-p "-i " arg)
      (fpga-manager-bstt--set :toplevel-local-code (substring arg 3)))
     ((string-prefix-p "-r " arg)
      (fpga-manager-bstt--set :toplevel-repeat (substring arg 3)))
     ((string-prefix-p "-c " arg)
      (fpga-manager-bstt--set :toplevel-config (substring arg 3)))))
  (setq fpga-manager-test-type 'bitstream
        fpga-manager-test-last-args (if (listp args) args (list args))))

(defun fpga-manager--bstt-sync-from-webapp (env args parsed-args)
  "Update BSTT webapp defaults from fpga-manager ENV, ARGS, and PARSED-ARGS."
  (fpga-manager-bstt--set :webapp-cli-code env)
  (when-let* ((test-ids (alist-get 'test-ids parsed-args)))
    (fpga-manager-bstt--set :webapp-batch-code test-ids))
  (when-let* ((browser (alist-get 'browser parsed-args)))
    (fpga-manager-bstt--set :webapp-browser browser))
  (when-let* ((repeat (alist-get 'repeat parsed-args)))
    (fpga-manager-bstt--set :webapp-repeat repeat))
  (setq fpga-manager-test-type 'webapp
        fpga-manager-test-last-args (if (listp args) args (list args))))

(defun fpga-manager-bstt--read-string-into (prompt key history)
  "Prompt with PROMPT, store into KEY, and record into HISTORY."
  ;; Use read-string to allow empty inputs (meaning "omit flag").
  (fpga-manager-bstt--set
   key
   (string-trim (read-string prompt (fpga-manager-bstt--get key) history))))

(defun fpga-manager-bstt--read-choice-into (prompt choices key history)
  "Prompt with PROMPT over CHOICES, store into KEY, record into HISTORY."
  ;; completing-read enforces a finite choice set when desired.
  (fpga-manager-bstt--set
   key
   (completing-read prompt choices nil t (fpga-manager-bstt--get key) history)))

;; BSTT Lock Command Helper Functions

(defun bstt/lock-set-port ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "Port (-p, blank to omit): " :lock-port 'fpga-manager-bstt-lock-port-history))

(defun bstt/lock-set-value ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "Lock (-l, blank to omit): " :lock-value 'fpga-manager-bstt-lock-value-history))

(defun bstt/lock-build-cmd ()
  (fpga-manager-bstt--sync-state-from-vars)
  (let ((args nil)
        (port (fpga-manager-bstt--get :lock-port))
        (lock (fpga-manager-bstt--get :lock-value)))
    ;; Only add flags if the user actually provided a value.
    (when (and port (not (string-empty-p port)))
      (setq args (append args (list "-p" port))))
    (when (and lock (not (string-empty-p lock)))
      (setq args (append args (list "-l" lock))))
    (fpga-manager--build-command
     (append (fpga-manager--command-parts fpga-manager-python-command)
             (list "linuxPC_Lock.py")
             args))))

(defun bstt/lock-run ()
  (interactive)
  (let* ((cmd (bstt/lock-build-cmd))
         (default-directory (fpga-manager--project-fpga-dir)))
    (fpga-manager--confirm-and-compile default-directory cmd)))

;; BSTT Webapp Compile Command Helper Functions

(defun bstt/webapp-compile-set-cli-code ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "CLI Code: " :webapp-cli-code 'fpga-manager-bstt-webapp-cli-code-history)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-set-batch-code ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "Batch Code: " :webapp-batch-code 'fpga-manager-bstt-webapp-batch-code-history)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-set-browser ()
  (interactive)
  ;; Reuse the same browser history used by fpga-manager transient.
  (fpga-manager-bstt--read-choice-into
   "Browser: "
   '("chrome" "firefox" "safari" "edge")
   :webapp-browser
   'fpga-manager-webapp-browser-history)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-set-repeat ()
  (interactive)
  ;; Reuse repeat history from test menu.
  (fpga-manager-bstt--read-string-into
   "Repeat count: " :webapp-repeat 'fpga-manager-test-repeat-history)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-build-cmd ()
  (fpga-manager-bstt--sync-state-from-vars)
  (fpga-manager--build-command
   (append (fpga-manager--command-parts fpga-manager-python-command)
           (list "main.py" "http"
                 (fpga-manager-bstt--get :webapp-cli-code)
                 (fpga-manager-bstt--get :webapp-batch-code)
                 "--browser" (fpga-manager-bstt--get :webapp-browser)
                 "--repeat" (fpga-manager-bstt--get :webapp-repeat)))))

(defun bstt/webapp-compile-run ()
  (interactive)
  (let* ((script-path (fpga-manager-find-webapp-script))
         (default-directory (file-name-directory script-path))
         (cmd (bstt/webapp-compile-build-cmd)))
    (fpga-manager--bstt-sync-test-menu 'webapp)
    (fpga-manager--confirm-and-compile default-directory cmd)))

;; BSTT Toplevel Command Helper Functions

(defun bstt/toplevel-set-cli-code ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "CLI Code (-p): " :toplevel-cli-code 'fpga-manager-bstt-toplevel-cli-code-history)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-set-local-code ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "Local Code (--local): " :toplevel-local-code 'fpga-manager-bstt-toplevel-local-code-history)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-set-config ()
  (interactive)
  (fpga-manager-bstt--read-string-into
   "Config (-c, blank to omit): " :toplevel-config 'fpga-manager-bstt-toplevel-config-history)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-set-repeat ()
  (interactive)
  ;; Reuse repeat history from test menu.
  (fpga-manager-bstt--read-string-into
   "Repeat (-r): " :toplevel-repeat 'fpga-manager-test-repeat-history)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-build-cmd ()
  (fpga-manager-bstt--sync-state-from-vars)
  (let ((args (append (fpga-manager--command-parts fpga-manager-python-command)
                      (list "toplevel.py" "-d" "-p" (fpga-manager-bstt--get :toplevel-cli-code)
                            "--local" (fpga-manager-bstt--get :toplevel-local-code)
                            "-r" (fpga-manager-bstt--get :toplevel-repeat)))))
    ;; Optional -c flag only if config was provided.
    (when-let* ((cfg (fpga-manager-bstt--get :toplevel-config)))
      (unless (string-empty-p cfg)
        (setq args (append args (list "-c" cfg)))))
    (fpga-manager--build-command args)))

(defun bstt/toplevel-run ()
  (interactive)
  (let* ((script-path (fpga-manager-find-toplevel-script))
         (default-directory (file-name-directory script-path))
         (cmd (bstt/toplevel-build-cmd)))
    (fpga-manager--bstt-sync-test-menu 'bitstream)
    (fpga-manager--confirm-and-compile default-directory cmd)))

;; BSTT Code Submission Check Command Helper Functions
(defun bstt/code-check-run ()
  (interactive)
  (let* ((script-path (fpga-manager-find-webapp-script))
         (default-directory (file-name-directory script-path))
         (cmd (fpga-manager--build-command
               (append (fpga-manager--command-parts fpga-manager-python-command)
                       (list "code_submission_check.py")))))
    (fpga-manager--confirm-and-compile default-directory cmd)))

(provide 'fpga-manager)
;;; lib-fpga-manager.el ends here
