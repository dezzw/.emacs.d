;;; fpga-manager.el --- Interactive FPGA management -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025
;; Author: Desmond Wang
;; Version: 2.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, hardware

;;; Commentary:

;; Interactive FPGA lock manager.  Usage: M-x fpga-manager-status
;;
;; Layout mirrors linuxPC_Lock.py section tables, with an extra MY FPGAs
;; table on top for hosts locked by you.  Keys: l/u/f lock, g refresh,
;; t test, ? menu, q quit.

;;; Code:

(require 'project)
(require 'transient)
(require 'subr-x)
(require 'cl-lib)

;;; Customization

;; User-facing customization group so options show under M-x customize-group.
(defgroup fpga-manager nil
  "Interactive FPGA lock management."
  :group 'tools
  :prefix "fpga-manager-")

;; Username used for filtering "My FPGAs". We avoid forcing a default here so that
;; first-time usage can auto-detect via $USER or `user-login-name`.
(defcustom fpga-manager-current-user nil
  "Current user name for the top MY FPGAs table.
Should match the username as shown in the FPGA table,
e.g., \"DESMOND.WANG\" or just \"DESMOND\".
If nil, auto-detect from $USER / `user-login-name'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Username"))
  :group 'fpga-manager)

;;; Faces

(defface fpga-manager-section-header
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section banners in FPGA manager.
Avoid :height here — scaled text breaks column alignment."
  :group 'fpga-manager)

(defface fpga-manager-table-header
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for per-table column header rows."
  :group 'fpga-manager)

(defface fpga-manager-separator
  '((t :inherit shadow))
  "Face for separators in FPGA manager."
  :group 'fpga-manager)

(defface fpga-manager-current-line
  '((t :inherit hl-line))
  "Face for the FPGA row under point."
  :group 'fpga-manager)

;;; Internal Variables

;; Alist of (ENV . USER) for lock/unlock checks at point.
(defvar fpga-manager--entries nil
  "Alist of (ENV . USER) from the last status refresh.")

(defvar-local fpga-manager--current-row-overlay nil
  "Overlay highlighting the FPGA row under point.")

(defconst fpga-manager--section-title-regexp
  "^-\\{10,\\}\\s-*\\(.+?\\)\\s-*-\\{10,\\}$"
  "Regexp matching linuxPC_Lock.py section banners.")

(defun fpga-manager--highlight-current-row ()
  "Highlight the FPGA data row under point."
  (when (derived-mode-p 'fpga-manager-mode)
    (unless fpga-manager--current-row-overlay
      (setq fpga-manager--current-row-overlay
            (make-overlay (point-min) (point-min)))
      (overlay-put fpga-manager--current-row-overlay
                   'face 'fpga-manager-current-line)
      (overlay-put fpga-manager--current-row-overlay 'priority 1000))
    (if (fpga-manager--get-env-at-point)
        (move-overlay fpga-manager--current-row-overlay
                      (line-beginning-position)
                      (line-end-position))
      (delete-overlay fpga-manager--current-row-overlay))))

;;; Parsing Functions

(defun fpga-manager--split-pipe-line (line)
  "Split a prettytable LINE into trimmed column strings, or nil."
  (when (string-prefix-p "|" line)
    (mapcar #'string-trim (cl-subseq (split-string line "|") 1 -1))))

(defun fpga-manager--header-row-p (cols)
  "Return non-nil when COLS is a table header row."
  (when-let* ((first (car cols)))
    (member (downcase first) '("host" "env"))))

(defun fpga-manager--cli-row-p (cols)
  "Return non-nil when COLS is a CLI data row."
  (when-let* ((first (car cols)))
    (and (string-match-p "\\`CLI[0-9]+\\'" first) t)))

(defun fpga-manager--parse-section-title (line)
  "Return section title from a banner LINE, or nil."
  (when (string-match fpga-manager--section-title-regexp line)
    (string-trim (match-string 1 line))))

(defun fpga-manager--parse-output (output)
  "Parse linuxPC_Lock.py OUTPUT into a list of sections.
Each section is (TITLE COLUMNS ROWS) where COLUMNS is a list of header
names and ROWS is a list of cell lists.  Table shapes are kept per
section (not merged)."
  (let (sections
        current-title
        current-headers
        current-rows)
    (cl-labels
        ((flush ()
           (when (and current-title current-headers)
             (push (list current-title
                         current-headers
                         (nreverse current-rows))
                   sections))
           (setq current-headers nil
                 current-rows nil)))
      (dolist (line (split-string output "\n"))
        (let ((title (fpga-manager--parse-section-title line))
              (cols (fpga-manager--split-pipe-line line)))
          (cond
           (title
            (flush)
            (setq current-title title))
           ((and current-title (fpga-manager--header-row-p cols))
            (setq current-headers cols
                  current-rows nil))
           ((and current-headers (fpga-manager--cli-row-p cols))
            (push cols current-rows)))))
      (flush))
    (nreverse sections)))

(defun fpga-manager--column-index (columns name &optional default)
  "Return index of NAME in COLUMNS (case-insensitive), or DEFAULT."
  (or (cl-position name columns :test #'string-equal-ignore-case)
      default))

(defun fpga-manager--compute-widths (columns rows)
  "Return display widths for COLUMNS based on header names and ROWS."
  (let ((widths (mapcar (lambda (col) (max 4 (string-width col))) columns)))
    (dolist (row rows)
      (dotimes (i (min (length widths) (length row)))
        (setcar (nthcdr i widths)
                (min 40 (max (nth i widths)
                             (string-width (format "%s" (nth i row))))))))
    widths))

(defun fpga-manager--pad-row (columns row)
  "Pad or trim ROW to match COLUMNS length."
  (let* ((n (length columns))
         (row (append row nil)))
    (while (< (length row) n)
      (setq row (append row '(""))))
    (cl-subseq row 0 n)))

(defun fpga-manager--format-cells (widths cells)
  "Format CELLS as a prettytable row using WIDTHS.
Uses `string-width' so padding matches display width."
  (concat "| "
          (mapconcat
           (lambda (pair)
             (let* ((width (car pair))
                    (text (format "%s" (cdr pair)))
                    (pad (max 0 (- width (string-width text)))))
               (concat text (make-string pad ?\s))))
           (cl-mapcar #'cons widths cells)
           " | ")
          " |"))

(defun fpga-manager--rule-line (widths)
  "Return a prettytable rule matching WIDTHS."
  (concat "+"
          (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+")
          "+"))

;;; Entry Accessors

(defun fpga-manager--get-env-at-point ()
  "Get the ENV (CLI*) name from the current row."
  (get-text-property (line-beginning-position) 'fpga-env))

(defun fpga-manager--entry-user (entry)
  "Get the user from ENTRY (ENV . USER)."
  (cdr entry))

(defun fpga-manager--entry-locked-p (entry)
  "Check if ENTRY is locked (has a user)."
  (and entry (not (string-empty-p (or (fpga-manager--entry-user entry) "")))))

(defun fpga-manager--entry-mine-p (entry)
  "Check if ENTRY belongs to current user."
  (when (and entry fpga-manager-current-user)
    (let ((user-field (downcase (string-trim (fpga-manager--entry-user entry))))
          (current-user (downcase (string-trim fpga-manager-current-user))))
      (and (not (string-empty-p user-field))
           (string-match-p (regexp-quote current-user) user-field)))))

(defun fpga-manager--merge-headers (columns headers)
  "Append any new names in HEADERS onto COLUMNS, preserving order."
  (let ((result (copy-sequence columns)))
    (dolist (header headers result)
      (unless (member header result)
        (setq result (append result (list header)))))))

(defun fpga-manager--align-row (headers columns values)
  "Map VALUES under HEADERS onto the global COLUMNS list."
  (let ((pairs (cl-mapcar #'cons headers values)))
    (mapcar (lambda (col)
              (or (cdr (assoc col pairs)) ""))
            columns)))

(defun fpga-manager--collect-mine-section (sections)
  "Build a MY FPGAs section from ROWS locked by the current user.
Uses the union of all section columns so every field remains visible.
Return (SECTION . MINE-ENVS), or nil when nothing is locked by you."
  (let (columns mine-rows mine-envs)
    (dolist (section sections)
      (cl-destructuring-bind (_title headers rows) section
        (setq columns (fpga-manager--merge-headers columns headers))
        (let ((user-idx (fpga-manager--column-index headers "User" 1)))
          (dolist (row rows)
            (let* ((env (car row))
                   (user (or (nth user-idx row) ""))
                   (entry (cons env user)))
              (when (and (fpga-manager--entry-mine-p entry)
                         (not (member env mine-envs)))
                (push env mine-envs)
                (push (fpga-manager--align-row headers columns row)
                      mine-rows)))))))
    (when mine-rows
      (cons (list "MY FPGAs" columns (nreverse mine-rows))
            (nreverse mine-envs)))))

(defun fpga-manager--sections-without-envs (sections envs)
  "Return SECTIONS with rows whose host is in ENVS removed.
Drops a section entirely when it has no remaining rows."
  (let (kept)
    (dolist (section sections (nreverse kept))
      (cl-destructuring-bind (title columns rows) section
        (let ((filtered (cl-remove-if (lambda (row)
                                        (member (car row) envs))
                                      rows)))
          (when filtered
            (push (list title columns filtered) kept)))))))

;;; Display

(defun fpga-manager--insert-section (title columns rows)
  "Insert one linuxPC_Lock.py-style table for TITLE."
  (let* ((rows (mapcar (lambda (row) (fpga-manager--pad-row columns row)) rows))
         (widths (fpga-manager--compute-widths columns rows))
         (banner (format "%s %s %s"
                         (make-string 35 ?-)
                         title
                         (make-string 35 ?-)))
         (rule (fpga-manager--rule-line widths))
         (user-idx (fpga-manager--column-index columns "User" 1)))
    (insert "\n")
    (insert (propertize banner 'face 'fpga-manager-section-header))
    (insert "\n")
    (insert (propertize rule 'face 'fpga-manager-separator))
    (insert "\n")
    (insert (propertize (fpga-manager--format-cells widths columns)
                        'face 'fpga-manager-table-header))
    (insert "\n")
    (insert (propertize rule 'face 'fpga-manager-separator))
    (insert "\n")
    (dolist (row rows)
      (let ((start (point))
            (env (car row))
            (user (or (nth user-idx row) "")))
        (insert (fpga-manager--format-cells widths row))
        (insert "\n")
        (add-text-properties
         start (point)
         (list 'fpga-env env
               'fpga-user user
               'fpga-entry (cons env user)
               'help-echo "RET/l lock, u unlock, f force-unlock"))))
    (insert (propertize rule 'face 'fpga-manager-separator))
    (insert "\n")))

(defun fpga-manager--render-sections (sections)
  "Erase buffer and render SECTIONS from `fpga-manager--parse-output'.
Prepends MY FPGAs and removes those hosts from the original tables."
  (let* ((inhibit-read-only t)
         (entries nil)
         (mine-info (fpga-manager--collect-mine-section sections))
         (mine (car mine-info))
         (mine-envs (cdr mine-info))
         (rest (fpga-manager--sections-without-envs sections mine-envs))
         (display (if mine (cons mine rest) rest)))
    (erase-buffer)
    (dolist (section display)
      (cl-destructuring-bind (title columns rows) section
        (fpga-manager--insert-section title columns rows)
        (let ((user-idx (fpga-manager--column-index columns "User" 1)))
          (dolist (row rows)
            (let* ((env (car row))
                   (user (or (nth user-idx row) "")))
              (unless (assoc env entries)
                (push (cons env user) entries)))))))
    (setq fpga-manager--entries (nreverse entries))
    (goto-char (point-min))
    (fpga-manager--highlight-current-row)))

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

(defun fpga-manager--lock-script-path ()
  "Return the absolute path to linuxPC_Lock.py in the current project."
  (expand-file-name "linuxPC_Lock.py" (fpga-manager--project-fpga-dir)))

;; Quote each part and re-join into a shell command string.
(defun fpga-manager--build-command (parts)
  "Build a shell command string from PARTS."
  (mapconcat #'shell-quote-argument parts " "))

(defun fpga-manager--ensure-list (value)
  "Return VALUE as a list."
  (if (listp value) value (list value)))

(defun fpga-manager--flatten-command-args (args)
  "Expand ARGS into shell-ready parts.
Each entry may already be a single argument or a short command fragment."
  (if (null args)
      nil
    (apply #'append
           (mapcar #'split-string-and-unquote
                   (let (parts)
                     (dolist (arg (fpga-manager--ensure-list args) (nreverse parts))
                       (unless (or (null arg) (string-empty-p arg))
                         (push arg parts))))))))

(defun fpga-manager--build-uv-run-command (parts &optional env-parts)
  "Build a `uv run` shell command from PARTS.
When ENV-PARTS is non-nil, run the full command under that environment."
  (let ((command (fpga-manager--build-command
                  (append '("uv" "run") parts))))
    (if env-parts
        (fpga-manager--build-command
         (append env-parts
                 (list (or shell-file-name "/bin/sh")
                       (or shell-command-switch "-c")
                       command)))
      command)))

(defun fpga-manager--join-shell-commands (&rest commands)
  "Join COMMANDS with `&&`, skipping any empty entries."
  (mapconcat #'identity
             (delq nil
                   (mapcar (lambda (command)
                             (unless (string-empty-p command)
                               command))
                           commands))
             " && "))

(defun fpga-manager--pipe-stdin-to-command (stdin command)
  "Wrap COMMAND so a shell reads STDIN from a printf of STDIN."
  (format "printf '%%s\\n' %s | %s"
          (shell-quote-argument stdin)
          command))

(defun fpga-manager--ensure-fpga-dir ()
  "Return the project `fpga/' directory, or signal `user-error'."
  (let ((dir (fpga-manager--project-fpga-dir)))
    (unless (file-directory-p dir)
      (user-error "No fpga/ directory under project root: %s"
                  (fpga-manager--project-root)))
    dir))

(defun fpga-manager--build-lock-state-command (env lock-value)
  "Build the lock command that sets ENV to LOCK-VALUE."
  (fpga-manager--build-uv-run-command
   (list (fpga-manager--lock-script-path)
         "-p" env
         "-l" lock-value)))

(defun fpga-manager--with-post-command (command post-command)
  "Run COMMAND, then POST-COMMAND, preserving useful failure status.
If COMMAND fails, return its status after POST-COMMAND runs. If COMMAND
succeeds, return POST-COMMAND's status."
  (format "{ %s; command_status=$?; %s; post_status=$?; if [ \"$command_status\" -ne 0 ]; then exit \"$command_status\"; fi; exit \"$post_status\"; }"
          command
          post-command))

(defun fpga-manager--build-webapp-session-command (env command &optional keep-locked)
  "Build a shell command that locks ENV, runs COMMAND, and maybe unlocks it.
When KEEP-LOCKED is non-nil, leave ENV locked after COMMAND exits."
  (fpga-manager--join-shell-commands
   (fpga-manager--build-lock-state-command env "1")
   (if keep-locked
       command
     (fpga-manager--with-post-command
      command
      (fpga-manager--build-lock-state-command env "0")))))

;; Buffer name for project-scoped compile output (tests/confirms).
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
  "Run linuxPC_Lock.py with ARGS synchronously and return output.
Uses `call-process' (no shell) so arguments stay safely quoted."
  (let* ((default-directory (fpga-manager--ensure-fpga-dir))
         (parts (append (list "linuxPC_Lock.py")
                        (fpga-manager--flatten-command-args args))))
    (with-temp-buffer
      (let ((status (apply #'call-process "uv" nil t nil "run" parts)))
        (unless (eq status 0)
          (message "linuxPC_Lock.py exited with status %s" status))
        (buffer-string)))))

(defun fpga-manager--on-compile-success (buffer-name callback)
  "Call CALLBACK after BUFFER-NAME finishes compiling successfully."
  (letrec ((hook-fn
            (lambda (buffer result)
              (when (equal (buffer-name buffer) buffer-name)
                (remove-hook 'compilation-finish-functions hook-fn)
                (when (string-match-p "\\`finished" result)
                  (funcall callback))))))
    (add-hook 'compilation-finish-functions hook-fn)))

;; Asynchronous compile-based execution gives us clickable errors and output.
(defun fpga-manager--run-script-compile (args &optional on-success stdin)
  "Run linuxPC_Lock.py with ARGS in compilation mode.
If ON-SUCCESS is provided, it will be called after successful compilation.
When STDIN is non-nil, feed it to the script (for interactive prompts).
ARGS should be a list of separate arguments (they are shell-quoted)."
  (let* ((default-directory (fpga-manager--ensure-fpga-dir))
         (parts (append (list "linuxPC_Lock.py")
                        (fpga-manager--flatten-command-args args)))
         (cmd (fpga-manager--build-uv-run-command parts))
         (compilation-buffer-name-function
          (lambda (_mode) "*FPGA Manager Output*")))
    (when stdin
      (setq cmd (fpga-manager--pipe-stdin-to-command stdin cmd)))
    (when on-success
      (fpga-manager--on-compile-success "*FPGA Manager Output*" on-success))
    (compile cmd)))

;;; Interactive Commands - Lock/Unlock

(defun fpga-manager--valid-env-p (env)
  "Return non-nil when ENV looks like a CLI host id."
  (and (stringp env)
       (string-match-p "\\`CLI[0-9]+\\'" env)
       t))

;; Helper wrapper so commands can share "get env at point" error handling.
(defun fpga-manager--with-env-at-point (action)
  "Execute ACTION with ENV at point if available.
ACTION is a function that takes ENV as argument."
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (if (fpga-manager--valid-env-p env)
          (funcall action env)
        (message "Invalid ENV at point: %S" env))
    (message "No ENV found at point")))

(defun fpga-manager--refresh-buffer (buffer)
  "Refresh FPGA status BUFFER when it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (fpga-manager-refresh))))

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
          (list "-p" env "-l" "1")
          (lambda () (fpga-manager--refresh-buffer status-buffer))))))))

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
          (list "-p" env "-l" "0")
          (lambda () (fpga-manager--refresh-buffer status-buffer))))))))

(defun fpga-manager-force-unlock ()
  "Force unlock the FPGA at point."
  (interactive)
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let ((entry (assoc env fpga-manager--entries)))
       (if (not (fpga-manager--entry-locked-p entry))
           (message "%s is not locked" env)
         ;; linuxPC_Lock.py also prompts [Y/N]; answer here and pipe it through.
         (when (y-or-n-p (format "Force unlock %s? " env))
           (let ((status-buffer (current-buffer)))
             (message "Force unlocking %s..." env)
             (fpga-manager--run-script-compile
              (list "-p" env "-l" "2")
              (lambda () (fpga-manager--refresh-buffer status-buffer))
              "y"))))))))

;;; Interactive Commands - View

(defun fpga-manager-refresh ()
  "Refresh the FPGA lock tables from linuxPC_Lock.py."
  (interactive)
  (message "Fetching FPGA status...")
  (fpga-manager--render-sections
   (fpga-manager--parse-output (fpga-manager--run-script-sync nil)))
  (message "Refreshed."))

(defun fpga-manager-set-user ()
  "Set the current user for the top MY FPGAs table."
  (interactive)
  (let ((user (read-string "Enter your username (as shown in FPGA table): "
                           fpga-manager-current-user)))
    (setq fpga-manager-current-user (string-trim user))
    (message "Current user set to: %s" fpga-manager-current-user)
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
        (debug nil)
        (headless nil)
        (keep-locked nil)
        (remaining '()))
    (dolist (arg (fpga-manager--ensure-list args))
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
        (setq debug t))
       ((string= arg "--headless")
        (setq headless t))
       ((string= arg "--keep-locked")
        (setq keep-locked t))
       (t
        (push arg remaining))))
    `((protocol . ,protocol)
      (test-ids . ,test-ids)
      (browser . ,browser)
      (repeat . ,repeat)
      (debug . ,debug)
      (headless . ,headless)
      (keep-locked . ,keep-locked)
      (remaining . ,(nreverse remaining)))))

(defun fpga-manager--convert-bitstream-args (args)
  "Convert transient ARGS to bitstream format.
Converts \\='-i\\=' to \\='--local\\=' for bitstream tests."
  (mapcar (lambda (arg)
            (if (string-prefix-p "-i " arg)
                (concat "--local " (substring arg 3))
              arg))
          (fpga-manager--ensure-list args)))

;; Build the command parts in a stable order so downstream scripts parse
;; positional arguments correctly.
(defun fpga-manager--build-webapp-command-parts (script-name env parsed-args)
  "Build webapp command parts from SCRIPT-NAME, ENV, and PARSED-ARGS."
  (let* ((protocol (alist-get 'protocol parsed-args))
         (debug (alist-get 'debug parsed-args))
         (repeat (alist-get 'repeat parsed-args))
         (browser (alist-get 'browser parsed-args))
         (test-ids (or (alist-get 'test-ids parsed-args) "webapp"))
         (remaining (alist-get 'remaining parsed-args)))
    (unless protocol
      (error "Protocol is required for webapp tests"))
    (let ((parts (list script-name protocol env test-ids)))
      ;; Positional arguments must come before optional flags.
      (when remaining
        (setq parts (append parts remaining)))
      ;; Now add optional flags
      (when debug
        (setq parts (append parts '("-d"))))
      (when repeat
        (setq parts (append parts (list "--repeat" repeat))))
      (when browser
        (setq parts (append parts (list "--browser" browser))))
      parts)))

(defun fpga-manager--build-webapp-main-command (script-path env parsed-args)
  "Build the main webapp command for SCRIPT-PATH, ENV, and PARSED-ARGS."
  (fpga-manager--build-uv-run-command
   (fpga-manager--build-webapp-command-parts
    (file-name-nondirectory script-path)
    env
    parsed-args)
   (when (alist-get 'headless parsed-args)
     '("env" "-u" "XDG_CURRENT_DESKTOP"))))

;;; Test Execution

(defun fpga-manager-run-test (&optional args)
  "Run test (bitstream or webapp) with ARGS."
  (interactive (list (transient-args 'fpga-manager-test-menu)))
  ;; Save arguments so the next menu invocation can reuse them.
  (fpga-manager--remember-test-args fpga-manager-test-type args)
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
            (cmd (fpga-manager--build-uv-run-command
                  (append (list (file-name-nondirectory script-path)
                                "-p" env)
                          (fpga-manager--flatten-command-args converted-args))))
            (compilation-buffer-name-function
             (lambda (_mode) "*FPGA Test Output*")))
       ;; Persist args into BSTT state so quick actions reuse them.
       (fpga-manager--bstt-sync-from-bitstream env args)
       (message "Running bitstream test: %s" cmd)
       (fpga-manager--on-compile-success
        "*FPGA Test Output*"
        (lambda () (fpga-manager--refresh-buffer status-buffer)))
       (compile cmd)))))

(defun fpga-manager-run-webapp-test (args)
  "Run webapp main.py test with ARGS."
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let* ((script-path (fpga-manager-find-webapp-script))
            (default-directory (file-name-directory script-path))
            (parsed (fpga-manager--parse-test-args args))
            (headless (alist-get 'headless parsed))
            (keep-locked (alist-get 'keep-locked parsed)))
       (let* ((cmd (fpga-manager--build-webapp-main-command
                    script-path env parsed))
              (full-cmd (fpga-manager--build-webapp-session-command
                         env cmd keep-locked))
              (compilation-buffer-name-function
               (lambda (_mode) "*FPGA Test Output*")))
         ;; Persist args into BSTT state so quick actions reuse them.
         (fpga-manager--bstt-sync-from-webapp env args parsed)
         ;; Headless mode drops the desktop variable to avoid GUI dependency.
         (message "Running webapp test%s%s: %s"
                  (if headless " (headless)" "")
                  (if keep-locked " (keeping host locked)" "")
                  full-cmd)
         (compile full-cmd))))))

;;; Transient Test Menu

;; The transient menu captures common flags and exposes richer options by
;; dynamically switching between bitstream and webapp sets.
(transient-define-prefix fpga-manager-test-menu ()
  "Transient menu for running FPGA tests."
  :value (lambda ()
           (or fpga-manager-test-last-args
               (if (eq fpga-manager-test-type 'webapp)
                   (fpga-manager--bstt-build-webapp-args)
                 (fpga-manager--bstt-build-bitstream-args))))
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
               (fpga-manager--state-read-string-into prompt :repeat)))
    ("-I" "Test ID" "-i "
     :class transient-option
     :prompt "Test ID: "
     :always-read t
     :reader (lambda (prompt _initial-input _history)
               (fpga-manager--state-read-string-into prompt :test-case)))]
   ["Bitstream Options"
    :if (lambda () (eq fpga-manager-test-type 'bitstream))
    ("-d" "Debug mode" "-d")
    ("-a" "Admin mode (force restricted CLI)" "-a")
    ("-l" "Log level" "-l "
     :class transient-option
     :prompt "Level: "
     :choices ("20" "50")
     :reader (lambda (prompt _initial-input _history)
               (fpga-manager--state-read-choice-into prompt
                                                     '("20" "50")
                                                     :log-level)))
    ("-c" "Custom args" "-c "
     :class transient-option
     :prompt "Custom (key:value): "
     :reader (lambda (prompt _initial-input _history)
               (fpga-manager--state-read-string-into prompt :custom)))]
   ["Webapp Options"
    :if (lambda () (eq fpga-manager-test-type 'webapp))
    ("-h" "Headless mode" "--headless")
    ("-k" "Keep host locked" "--keep-locked")
    ("-p" "Protocol" "--protocol="
     :class transient-option
     :prompt "Protocol (http/https): "
     :choices ("http" "https")
     :reader (lambda (prompt _initial-input _history)
               (fpga-manager--state-read-choice-into prompt
                                                     '("http" "https")
                                                     :protocol)))
    ("-b" "Browser" "--browser="
     :class transient-option
     :prompt "Browser: "
     :choices ("chrome" "firefox" "safari" "edge")
     :reader (lambda (prompt _initial-input _history)
               (fpga-manager--state-read-choice-into prompt
                                                     '("chrome" "firefox" "safari" "edge")
                                                     :webapp-browser)))]]
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

(defvar fpga-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
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

(define-derived-mode fpga-manager-mode special-mode "FPGA-Manager"
  "Major mode for managing FPGA locks interactively.

Shows the same section tables as linuxPC_Lock.py (BQE Hosts, Agents, ...),
plus a MY FPGAs table for hosts locked by you.

\\{fpga-manager-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (fpga-manager-refresh)))
  (add-hook 'post-command-hook
            #'fpga-manager--highlight-current-row
            nil
            t))

;;; Entry Point

(defconst fpga-manager--status-buffer-name "*FPGA Manager*"
  "Shared status buffer. Lock state is global, so one buffer is enough.")

;;;###autoload
(defun fpga-manager-status ()
  "Display FPGA Manager status in a shared interactive buffer."
  (interactive)
  (let* ((project-root (fpga-manager--project-root))
         (buffer (get-buffer-create fpga-manager--status-buffer-name)))
    (with-current-buffer buffer
      ;; Scripts still live under the caller's project `fpga/' tree.
      (setq default-directory project-root)
      (unless (derived-mode-p 'fpga-manager-mode)
        (fpga-manager-mode)
        (fpga-manager--detect-current-user)))
    (switch-to-buffer buffer)
    (fpga-manager-refresh)
    (message "Press '?' for help")))

;;; BSTT Helper Commands

(defun fpga-manager--state-get (key)
  "Get KEY from `fpga-manager--state'."
  (plist-get fpga-manager--state key))

(defun fpga-manager--state-set (key value)
  "Set KEY to VALUE in `fpga-manager--state'."
  (setq fpga-manager--state
        (plist-put fpga-manager--state key value))
  value)

(defun fpga-manager--state-get-string (key)
  "Get KEY from shared state when it is a non-empty string."
  (let ((value (fpga-manager--state-get key)))
    (when (and (stringp value)
               (not (string-empty-p value)))
      value)))

(defun fpga-manager--push-arg-if-value (args prefix value)
  "Push PREFIX plus VALUE onto ARGS when VALUE is non-empty."
  (if value
      (cons (concat prefix value) args)
    args))

(defun fpga-manager--remember-test-args (type args)
  "Remember transient TYPE and ARGS for the next test menu invocation."
  (setq fpga-manager-test-type type
        fpga-manager-test-last-args (fpga-manager--ensure-list args)))

(defun fpga-manager--sync-state-from-alist (mapping parsed-args)
  "Apply MAPPING from PARSED-ARGS keys to state plist keys."
  (dolist (entry mapping)
    (when-let* ((value (alist-get (car entry) parsed-args)))
      (fpga-manager--state-set (cdr entry) value))))

(defun fpga-manager-state-get (key)
  "Get shared fpga-manager state value for KEY."
  (fpga-manager--state-get key))

(defun fpga-manager--bstt-build-bitstream-args ()
  "Build fpga-manager transient args from BSTT bitstream state."
  (let ((args '("-d")))
    (setq args (fpga-manager--push-arg-if-value
                args "-i " (fpga-manager--state-get-string :test-case)))
    (setq args (fpga-manager--push-arg-if-value
                args "-l " (fpga-manager--state-get-string :log-level)))
    (setq args (fpga-manager--push-arg-if-value
                args "-r " (fpga-manager--state-get-string :repeat)))
    (setq args (fpga-manager--push-arg-if-value
                args "-c " (fpga-manager--state-get-string :custom)))
    (nreverse args)))

(defun fpga-manager--bstt-build-webapp-args ()
  "Build fpga-manager transient args from BSTT webapp state."
  (let ((args nil))
    (push (concat "--protocol="
                  (or (fpga-manager--state-get-string :protocol) "http"))
          args)
    (setq args (fpga-manager--push-arg-if-value
                args "-i " (fpga-manager--state-get-string :test-case)))
    (setq args (fpga-manager--push-arg-if-value
                args "-r " (fpga-manager--state-get-string :repeat)))
    (setq args (fpga-manager--push-arg-if-value
                args "--browser=" (fpga-manager--state-get-string :webapp-browser)))
    (when (fpga-manager--state-get :keep-locked)
      (push "--keep-locked" args))
    (nreverse args)))

(defun fpga-manager--bstt-webapp-parsed-args ()
  "Build canonical parsed webapp args from shared BSTT state."
  `((protocol . ,(or (fpga-manager--state-get-string :protocol) "http"))
    (test-ids . ,(or (fpga-manager--state-get-string :test-case) "webapp"))
    (browser . ,(fpga-manager--state-get-string :webapp-browser))
    (repeat . ,(fpga-manager--state-get-string :repeat))
    (debug . nil)
    (headless . nil)
    (keep-locked . ,(fpga-manager--state-get :keep-locked))
    (remaining . nil)))

(defun fpga-manager--bstt-sync-test-menu (type)
  "Sync fpga-manager test menu defaults from BSTT state."
  (setq fpga-manager-test-type type
        fpga-manager-test-last-args
        (if (eq type 'webapp)
            (fpga-manager--bstt-build-webapp-args)
          (fpga-manager--bstt-build-bitstream-args))))

(defun fpga-manager--bstt-sync-from-bitstream (env args)
  "Update BSTT bitstream defaults from fpga-manager ENV and ARGS."
  (fpga-manager--state-set :port env)
  (dolist (arg (fpga-manager--ensure-list args))
    (cond
     ((string-prefix-p "-i " arg)
      (fpga-manager--state-set :test-case (substring arg 3)))
     ((string-prefix-p "-r " arg)
      (fpga-manager--state-set :repeat (substring arg 3)))
     ((string-prefix-p "-l " arg)
      (fpga-manager--state-set :log-level (substring arg 3)))
     ((string-prefix-p "-c " arg)
      (fpga-manager--state-set :custom (substring arg 3)))))
  (fpga-manager--remember-test-args 'bitstream args))

(defun fpga-manager--bstt-sync-from-webapp (env args parsed-args)
  "Update BSTT webapp defaults from fpga-manager ENV, ARGS, and PARSED-ARGS."
  (fpga-manager--state-set :port env)
  (fpga-manager--sync-state-from-alist
   '((protocol . :protocol)
     (test-ids . :test-case)
     (browser . :webapp-browser)
     (repeat . :repeat))
   parsed-args)
  (fpga-manager--state-set :keep-locked (and (alist-get 'keep-locked parsed-args) t))
  (fpga-manager--remember-test-args 'webapp args))

(defun fpga-manager--state-read-string-into (prompt key)
  "Prompt with PROMPT and store the entered value into KEY."
  ;; Use read-string to allow empty inputs (meaning "omit flag").
  (fpga-manager--state-set
   key
   (string-trim (read-string prompt (fpga-manager--state-get key)))))

(defun fpga-manager--state-read-choice-into (prompt choices key)
  "Prompt with PROMPT over CHOICES and store the selected value into KEY."
  ;; completing-read enforces a finite choice set when desired.
  (fpga-manager--state-set
   key
   (completing-read prompt choices nil t (fpga-manager--state-get key))))

;; BSTT Lock Command Helper Functions

(defun bstt/lock-set-port ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Port (-p, blank to omit): " :port))

(defun bstt/lock-set-value ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Lock (-l, blank to omit): " :lock-status))

(defun bstt/lock-build-cmd ()
  (let ((args nil)
        (port (fpga-manager--state-get :port))
        (lock (fpga-manager--state-get :lock-status)))
    ;; Only add flags if the user actually provided a value.
    (when (and port (not (string-empty-p port)))
      (setq args (append args (list "-p" port))))
    (when (and lock (not (string-empty-p lock)))
      (setq args (append args (list "-l" lock))))
    (fpga-manager--build-uv-run-command
     (append (list "linuxPC_Lock.py") args))))

(defun bstt/lock-run ()
  (interactive)
  (let* ((cmd (bstt/lock-build-cmd))
         (default-directory (fpga-manager--ensure-fpga-dir)))
    (fpga-manager--confirm-and-compile default-directory cmd)))

;; BSTT Webapp Compile Command Helper Functions

(defun bstt/webapp-compile-set-cli-code ()
  (interactive)
  (fpga-manager--state-read-string-into
   "CLI Code: " :port)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-set-batch-code ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Batch Code: " :test-case)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-set-browser ()
  (interactive)
  (fpga-manager--state-read-choice-into
   "Browser: "
   '("chrome" "firefox" "safari" "edge")
   :webapp-browser)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-set-repeat ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Repeat count: " :repeat)
  (fpga-manager--bstt-sync-test-menu 'webapp))

(defun bstt/webapp-compile-build-cmd ()
  (let* ((env (fpga-manager--state-get :port))
         (parsed-args (fpga-manager--bstt-webapp-parsed-args))
         (script-path (fpga-manager-find-webapp-script))
         (main-cmd (fpga-manager--build-webapp-main-command
                    script-path env parsed-args)))
    (fpga-manager--build-webapp-session-command
     env
     main-cmd
     (alist-get 'keep-locked parsed-args))))

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
  (fpga-manager--state-read-string-into
   "CLI Code (-p): " :port)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-set-local-code ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Local Code (--local): " :test-case)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-set-config ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Config (-c, blank to omit): " :custom)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-set-repeat ()
  (interactive)
  (fpga-manager--state-read-string-into
   "Repeat (-r): " :repeat)
  (fpga-manager--bstt-sync-test-menu 'bitstream))

(defun bstt/toplevel-build-cmd ()
  (let ((args (list "toplevel.py" "-d" "-p" (fpga-manager--state-get :port)
                    "--local" (fpga-manager--state-get :test-case)
                    "-r" (fpga-manager--state-get :repeat))))
    ;; Optional -c flag only if config was provided.
    (when-let* ((cfg (fpga-manager--state-get :custom)))
      (unless (string-empty-p cfg)
        (setq args (append args (list "-c" cfg)))))
    (fpga-manager--build-uv-run-command args)))

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
         (cmd (fpga-manager--build-uv-run-command
               (list "code_submission_check.py"))))
    (fpga-manager--confirm-and-compile default-directory cmd)))

(provide 'fpga-manager)
;;; fpga-manager.el ends here
