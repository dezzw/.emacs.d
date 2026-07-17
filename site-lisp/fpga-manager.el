;;; fpga-manager.el --- Interactive FPGA management -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025
;; Author: Desmond Wang
;; Version: 2.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, hardware

;;; Commentary:

;; Interactive interface for managing FPGA locks.
;; Usage: M-x fpga-manager-status, then ? for the transient menu.
;;
;; The status buffer shows one global MY FPGAs block, free hosts grouped by
;; linuxPC_Lock.py section, and every other locked host in a global IN USE
;; block at the bottom.

;;; Code:

(require 'tabulated-list)
(require 'project)
(require 'transient)
(require 'subr-x)

;;; Customization

(defgroup fpga-manager nil
  "Interactive FPGA lock management."
  :group 'tools
  :prefix "fpga-manager-")

(defcustom fpga-manager-current-user nil
  "Username for filtering \\='My FPGAs\\='.
When nil, auto-detect from $USER or `user-login-name'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Username"))
  :group 'fpga-manager)

(defcustom fpga-manager-sync-before-mutate t
  "Run `uv sync' before lock/unlock and test commands.
Status refreshes never run `uv sync'."
  :type 'boolean
  :group 'fpga-manager)

;;; Constants and variables

(defconst fpga-manager--section-title-regexp
  "^-\\{10,\\}\\s-*\\(.+?\\)\\s-*-\\{10,\\}$"
  "Regexp for linuxPC_Lock.py section banners.")

(defconst fpga-manager--separator-line (make-string 135 ?─))
(defconst fpga-manager--manager-output-buffer "*FPGA Manager Output*")
(defconst fpga-manager--test-output-buffer "*FPGA Test Output*")

(defvar fpga-manager--entries nil)
(defvar fpga-manager-test-type 'bitstream)
(defvar fpga-manager-test-last-args nil)
(defvar fpga-manager--state
  '(:port "cli455"
          :lock-status ""
          :test-case "B000006"
          :protocol "http"
          :webapp-browser "chrome"
          :log-level ""
          :repeat "1"
          :keep-locked t
          :custom ""))

(defvar-local fpga-manager--current-row-overlay nil)
(defvar-local fpga-manager--highlight-line nil)

;;; Faces

(defface fpga-manager-section-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for section headers."
  :group 'fpga-manager)

(defface fpga-manager-separator
  '((t :inherit shadow))
  "Face for separators."
  :group 'fpga-manager)

(defface fpga-manager-highlight
  '((t :inherit success))
  "Face for the current FPGA row."
  :group 'fpga-manager)

;;; Shared state

(defun fpga-manager--state-get (key)
  (plist-get fpga-manager--state key))

(defun fpga-manager--state-set (key value)
  (setq fpga-manager--state (plist-put fpga-manager--state key value))
  value)

(defun fpga-manager-state-get (key)
  "Get shared fpga-manager state value for KEY."
  (fpga-manager--state-get key))

(defun fpga-manager--state-get-string (key)
  (let ((value (fpga-manager--state-get key)))
    (when (and (stringp value) (not (string-empty-p value)))
      value)))

(defun fpga-manager--state-read-string-into (prompt key)
  (fpga-manager--state-set
   key
   (string-trim (read-string prompt (fpga-manager--state-get key)))))

(defun fpga-manager--state-read-choice-into (prompt choices key)
  (fpga-manager--state-set
   key
   (completing-read prompt choices nil t (fpga-manager--state-get key))))

(defun fpga-manager--ensure-list (value)
  (if (listp value) value (list value)))

(defun fpga-manager--push-arg-if-value (args prefix value)
  (if value (cons (concat prefix value) args) args))

(defun fpga-manager--remember-test-args (type args)
  (setq fpga-manager-test-type type
        fpga-manager-test-last-args (fpga-manager--ensure-list args)))

(defun fpga-manager--bstt-build-bitstream-args ()
  (let ((args '("-d")))
    (dolist (pair '((:test-case . "-i ")
                    (:log-level . "-l ")
                    (:repeat . "-r ")
                    (:custom . "-c ")))
      (setq args (fpga-manager--push-arg-if-value
                  args (cdr pair) (fpga-manager--state-get-string (car pair)))))
    (nreverse args)))

(defun fpga-manager--bstt-build-webapp-args ()
  (let ((args (list (concat "--protocol="
                            (or (fpga-manager--state-get-string :protocol) "http")))))
    (dolist (pair '((:test-case . "-i ")
                    (:repeat . "-r ")
                    (:webapp-browser . "--browser=")))
      (setq args (fpga-manager--push-arg-if-value
                  args (cdr pair) (fpga-manager--state-get-string (car pair)))))
    (when (fpga-manager--state-get :keep-locked)
      (push "--keep-locked" args))
    (nreverse args)))

(defun fpga-manager--bstt-webapp-parsed-args ()
  `((protocol . ,(or (fpga-manager--state-get-string :protocol) "http"))
    (test-ids . ,(or (fpga-manager--state-get-string :test-case) "webapp"))
    (browser . ,(fpga-manager--state-get-string :webapp-browser))
    (repeat . ,(fpga-manager--state-get-string :repeat))
    (debug . nil)
    (headless . nil)
    (keep-locked . ,(fpga-manager--state-get :keep-locked))
    (remaining . nil)))

(defun fpga-manager--bstt-sync-test-menu (type)
  (setq fpga-manager-test-type type
        fpga-manager-test-last-args
        (if (eq type 'webapp)
            (fpga-manager--bstt-build-webapp-args)
          (fpga-manager--bstt-build-bitstream-args))))

(defun fpga-manager--bstt-edit-field (type prompt key &optional choices)
  "Prompt for KEY, store it, and refresh the test menu for TYPE."
  (if choices
      (fpga-manager--state-read-choice-into prompt choices key)
    (fpga-manager--state-read-string-into prompt key))
  (fpga-manager--bstt-sync-test-menu type))

(defun fpga-manager--bstt-sync-from-bitstream (env args)
  (fpga-manager--state-set :port env)
  (dolist (arg (fpga-manager--ensure-list args))
    (cond
     ((string-prefix-p "-i " arg) (fpga-manager--state-set :test-case (substring arg 3)))
     ((string-prefix-p "-r " arg) (fpga-manager--state-set :repeat (substring arg 3)))
     ((string-prefix-p "-l " arg) (fpga-manager--state-set :log-level (substring arg 3)))
     ((string-prefix-p "-c " arg) (fpga-manager--state-set :custom (substring arg 3)))))
  (fpga-manager--remember-test-args 'bitstream args))

(defun fpga-manager--bstt-sync-from-webapp (env args parsed-args)
  (fpga-manager--state-set :port env)
  (dolist (pair '((protocol . :protocol)
                  (test-ids . :test-case)
                  (browser . :webapp-browser)
                  (repeat . :repeat)))
    (when-let* ((value (alist-get (car pair) parsed-args)))
      (fpga-manager--state-set (cdr pair) value)))
  (fpga-manager--state-set :keep-locked (and (alist-get 'keep-locked parsed-args) t))
  (fpga-manager--remember-test-args 'webapp args))

;;; Parsing and table data

(defun fpga-manager--cli-from-id (id)
  "Return the CLI env name extracted from display id ID."
  (if (and (stringp id) (string-match "\\`\\(CLI[0-9]+\\)" id))
      (match-string 1 id)
    id))

(defun fpga-manager--current-user ()
  "Return the downcased username used for \\='My FPGAs\\=' filtering."
  (let ((user (or fpga-manager-current-user (getenv "USER") (user-login-name))))
    (and user (not (string-empty-p user))
         (downcase (string-trim user)))))

(defun fpga-manager--normalize-user (user)
  "Return USER as a lowercase alphanumeric string for fuzzy matching."
  (downcase (replace-regexp-in-string "[^[:alnum:]]+" "" user)))

(defun fpga-manager--user-matches-p (table-user)
  "Return non-nil when TABLE-USER belongs to `fpga-manager--current-user'."
  (let ((current (fpga-manager--normalize-user (or (fpga-manager--current-user) "")))
        (table (fpga-manager--normalize-user table-user)))
    (and (not (string-empty-p current))
         (not (string-empty-p table))
         (or (string-match-p (regexp-quote current) table)
             (string-match-p (regexp-quote table) current)))))

(defun fpga-manager--trim-field (value)
  "Return VALUE trimmed, or \"\" when VALUE is nil."
  (if value (string-trim value) ""))

(defun fpga-manager--parse-section-title (line)
  "Return the section title from a linuxPC_Lock.py banner LINE."
  (when (string-match fpga-manager--section-title-regexp line)
    (fpga-manager--trim-field (match-string 1 line))))

(defun fpga-manager--parse-pipe-row (line)
  "Parse a CLI table LINE into (env user date reserved motherboard cards model).
Handles both 5- and 7-column linuxPC_Lock.py tables."
  (when (and (string-prefix-p "|" line)
             (not (string-prefix-p "+" line)))
    (let ((parts (mapcar #'fpga-manager--trim-field (cdr (split-string line "|")))))
      (when (and parts (string-match-p "^CLI[0-9]+" (car parts)))
        (list (car parts)
              (or (nth 1 parts) "")
              (or (nth 2 parts) "")
              (or (nth 3 parts) "")
              (or (nth 4 parts) "")
              (or (nth 5 parts) "")
              (or (nth 6 parts) ""))))))

(defun fpga-manager--parse-output (output)
  "Parse all linuxPC_Lock.py sections from OUTPUT.
Return an alist with keys `sections' and `rows'."
  (let (rows section-order current-section)
    (dolist (line (split-string output "\n"))
      (let ((section (fpga-manager--parse-section-title line)))
        (cond
         (section
          (setq current-section section)
          (unless (member section section-order)
            (setq section-order (append section-order (list section)))))
         ((and current-section (fpga-manager--parse-pipe-row line))
          (push (cons current-section (fpga-manager--parse-pipe-row line)) rows)))))
    `((sections . ,section-order)
      (rows . ,(nreverse rows)))))

(defun fpga-manager--rows-for-section (rows section)
  "Return data rows in ROWS that belong to SECTION."
  (let (section-rows)
    (dolist (item rows (nreverse section-rows))
      (when (equal (car item) section)
        (push (cdr item) section-rows)))))

(defun fpga-manager--row->entry (env-id row)
  "Convert ROW into a tabulated-list entry with id ENV-ID."
  (list env-id (vconcat row)))

(defun fpga-manager--display-id (section env)
  "Return a unique tabulated-list id for ENV in SECTION."
  (if (string= section "Full Summary") env (concat env "@" section)))

(defun fpga-manager--register-entry (entries env row)
  "Add ENV/ROW to ENTRIES when ENV is not yet present."
  (unless (assoc env entries)
    (push (fpga-manager--row->entry env row) entries))
  entries)

(defun fpga-manager--entry-env (entry)
  "Return the CLI env name from display ENTRY."
  (fpga-manager--cli-from-id (car entry)))

(defun fpga-manager--entry-user (entry)
  "Return the lock owner from display ENTRY, or \"\" when free."
  (when entry
    (fpga-manager--trim-field (aref (cadr entry) 1))))

(defun fpga-manager--entry-locked-p (entry)
  "Return non-nil when ENTRY represents a locked host."
  (and entry (not (string-empty-p (fpga-manager--entry-user entry)))))

(defun fpga-manager--partition-entries (entries)
  "Return (mine free in-use) lists split from ENTRIES."
  (let ((mine '()) (in-use '()) (free '()))
    (dolist (entry entries)
      (let ((user (fpga-manager--entry-user entry)))
        (cond
         ((and (not (string-empty-p user))
               (fpga-manager--user-matches-p user))
          (push entry mine))
         ((not (string-empty-p user)) (push entry in-use))
         (t (push entry free)))))
    (list (nreverse mine) (nreverse free) (nreverse in-use))))

(defun fpga-manager--make-section-header (title)
  (list (concat "SECTION:" title) (vector title "" "" "" "" "" "")))

(defun fpga-manager--make-separator ()
  (list "SECTION:separator" (vector fpga-manager--separator-line "" "" "" "" "" "")))

(defun fpga-manager--section-block (title entries)
  "Return display rows for section TITLE followed by ENTRIES."
  (append (list (fpga-manager--make-separator)
                (fpga-manager--make-section-header title)
                (fpga-manager--make-separator))
          entries))

(defun fpga-manager--entries-for-section (section rows)
  "Return tabulated-list entries for SECTION in ROWS."
  (mapcar (lambda (row)
            (fpga-manager--row->entry
             (fpga-manager--display-id section (car row))
             row))
          (fpga-manager--rows-for-section rows section)))

(defun fpga-manager--seed-registry (entries rows)
  "Populate ENTRIES from Full Summary rows so registry columns stay rich."
  (dolist (row (fpga-manager--rows-for-section rows "Full Summary"))
    (setq entries (fpga-manager--register-entry entries (car row) row)))
  entries)

(defun fpga-manager--collect-envs (section-entries envs)
  "Add env names from SECTION-ENTRIES to ENVS without duplicates."
  (dolist (entry section-entries envs)
    (let ((env (fpga-manager--entry-env entry)))
      (unless (member env envs)
        (setq envs (cons env envs)))))
  envs)

(defun fpga-manager--filter-entries-by-envs (entries excluded-envs)
  "Remove entries whose env appears in EXCLUDED-ENVS."
  (if (null excluded-envs)
      entries
    (let (kept)
      (dolist (entry entries)
        (unless (member (fpga-manager--entry-env entry) excluded-envs)
          (push entry kept)))
      (nreverse kept))))

(defun fpga-manager--registry-display-rows (registry envs)
  "Return tabulated-list rows for ENVS using columns from REGISTRY."
  (mapcar (lambda (env)
            (let ((entry (assoc env registry)))
              (list env (cadr entry))))
          envs))

(defun fpga-manager--build-display-entries (parsed)
  "Build tabulated-list rows and the lock lookup table from PARSED output.

The buffer shows one global MY FPGAs block, free hosts grouped by section,
and one global IN USE block at the bottom."
  (let* ((section-order (cdr (assoc 'sections parsed)))
         (rows (cdr (assoc 'rows parsed)))
         (entries nil)
         (global-mine-envs nil)
         (global-in-use-envs nil)
         (display nil))
    (setq entries (fpga-manager--seed-registry entries rows))
    (dolist (section section-order)
      (dolist (row (fpga-manager--rows-for-section rows section))
        (setq entries (fpga-manager--register-entry entries (car row) row))))
    (dolist (section section-order)
      (let ((section-entries (fpga-manager--entries-for-section section rows)))
        (when section-entries
          (let ((parts (fpga-manager--partition-entries section-entries)))
            (setq global-mine-envs
                  (fpga-manager--collect-envs (nth 0 parts) global-mine-envs))
            (setq global-in-use-envs
                  (fpga-manager--collect-envs (nth 2 parts) global-in-use-envs))))))
    (let* ((mine-envs (reverse global-mine-envs))
           (in-use-envs (reverse global-in-use-envs))
           (excluded-envs (append mine-envs in-use-envs))
           (global-mine (fpga-manager--registry-display-rows entries mine-envs)))
      (when global-mine
        (setq display (append display
                              (fpga-manager--section-block "  MY FPGAs"
                                                           global-mine))))
      (dolist (section (reverse section-order))
        (let* ((parts (fpga-manager--partition-entries
                       (fpga-manager--entries-for-section section rows)))
               (visible-free (fpga-manager--filter-entries-by-envs
                              (nth 1 parts) excluded-envs)))
          (when visible-free
            (setq display (append display
                                (fpga-manager--section-block
                                 (format "  %s" section)
                                 visible-free))))))
      (let ((global-in-use (fpga-manager--registry-display-rows
                            entries in-use-envs)))
        (when global-in-use
          (setq display (append display
                                (fpga-manager--section-block "  IN USE"
                                                             global-in-use)))))
      (cons entries display))))

(defun fpga-manager--get-env-at-point ()
  "Return the CLI env at point, even for section-specific display ids."
  (when-let* ((id (tabulated-list-get-id)))
    (unless (string-prefix-p "SECTION:" id)
      (fpga-manager--cli-from-id id))))

(defun fpga-manager--count-locked-hosts (entries)
  "Return how many registry ENTRIES are currently locked."
  (let ((count 0))
    (dolist (entry entries)
      (when (fpga-manager--entry-locked-p entry)
        (setq count (1+ count))))
    count))

;;; Shell commands

(defun fpga-manager--project-root ()
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun fpga-manager--project-fpga-dir ()
  (expand-file-name "fpga" (fpga-manager--project-root)))

(defun fpga-manager--find-script (subdir filename)
  (let* ((root (fpga-manager--project-root))
         (script (expand-file-name (concat subdir "/" filename) root)))
    (if (file-exists-p script)
        script
      (error "Could not find %s/%s under %s" subdir filename root))))

(defun fpga-manager--build-command (parts)
  (mapconcat #'shell-quote-argument parts " "))

(defun fpga-manager--flatten-command-args (args)
  (apply #'append
         (mapcar #'split-string-and-unquote
                 (let (parts)
                   (dolist (arg (fpga-manager--ensure-list args) (nreverse parts))
                     (unless (string-empty-p arg)
                       (push arg parts)))))))

(defun fpga-manager--build-uv-run-command (parts &optional env-parts)
  (let ((command (fpga-manager--build-command (append '("uv" "run") parts))))
    (if env-parts
        (fpga-manager--build-command
         (append env-parts
                 (list (or shell-file-name "/bin/sh")
                       (or shell-command-switch "-c")
                       command)))
      command)))

(defun fpga-manager--join-shell-commands (&rest commands)
  (mapconcat #'identity
             (delq nil
                   (mapcar (lambda (command)
                             (unless (string-empty-p command) command))
                           commands))
             " && "))

(defun fpga-manager--maybe-with-uv-sync (sync &rest commands)
  (if sync
      (apply #'fpga-manager--join-shell-commands
             (fpga-manager--build-command '("uv" "sync"))
             commands)
    (apply #'fpga-manager--join-shell-commands commands)))

(defun fpga-manager--lock-script-command (args &optional sync)
  (fpga-manager--maybe-with-uv-sync
   sync
   (fpga-manager--build-uv-run-command
    (append (list (expand-file-name "linuxPC_Lock.py"
                                    (fpga-manager--project-fpga-dir)))
            (fpga-manager--flatten-command-args args)))))

(defun fpga-manager--with-post-command (command post-command)
  (format "{ %s; command_status=$?; %s; post_status=$?; if [ \"$command_status\" -ne 0 ]; then exit \"$command_status\"; fi; exit \"$post_status\"; }"
          command post-command))

(defun fpga-manager--build-webapp-session-command (env command &optional keep-locked)
  (let ((lock (lambda (value)
                (fpga-manager--lock-script-command (list "-p" env "-l" value)))))
    (fpga-manager--maybe-with-uv-sync
     fpga-manager-sync-before-mutate
     (funcall lock "1")
     (if keep-locked
         command
       (fpga-manager--with-post-command command (funcall lock "0"))))))

(defun fpga-manager--confirm-and-compile (default-dir cmd)
  (let* ((project-root (fpga-manager--project-root))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s_compilation*"
                    (file-name-nondirectory (directory-file-name project-root))))))
    (when (yes-or-no-p (format "Run command in %s: %s ?" default-dir cmd))
      (let ((default-directory default-dir))
        (compile cmd)))))

(defun fpga-manager--with-compilation-buffer (buffer-name thunk)
  (let ((compilation-buffer-name-function (lambda (_mode) buffer-name)))
    (funcall thunk)))

(defun fpga-manager--on-compile-success (compilation-buffer-name status-buffer)
  "After a successful compile, refresh STATUS-BUFFER."
  (letrec ((hook-fn
            (lambda (buffer result)
              (when (equal (buffer-name buffer) compilation-buffer-name)
                (remove-hook 'compilation-finish-functions hook-fn)
                (when (string-match "^finished" result)
                  (fpga-manager--refresh-buffer status-buffer))))))
    (add-hook 'compilation-finish-functions hook-fn)))

(defun fpga-manager--run-lock-script (args &optional sync status-buffer)
  (let ((default-directory (fpga-manager--project-fpga-dir)))
    (fpga-manager--with-compilation-buffer fpga-manager--manager-output-buffer
                                           (lambda ()
                                             (when status-buffer
                                               (fpga-manager--on-compile-success
                                                fpga-manager--manager-output-buffer
                                                status-buffer))
                                             (compile (fpga-manager--lock-script-command args sync))))))

;;; Interactive commands

(defun fpga-manager--with-env-at-point (action)
  (if-let* ((env (fpga-manager--get-env-at-point)))
      (funcall action env)
    (message "No ENV found at point")))

(defun fpga-manager--refresh-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (fpga-manager-refresh))))

(defun fpga-manager--set-lock-state (lock-value &optional force-p)
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let ((entry (assoc env fpga-manager--entries))
           (status-buffer (current-buffer)))
       (cond
        (force-p
         (when (yes-or-no-p (format "Force unlock %s? " env))
           (message "Force unlocking %s..." env)
           (fpga-manager--run-lock-script (list "-p" env "-l" "2")
                                          fpga-manager-sync-before-mutate
                                          status-buffer)))
        ((and (= lock-value 1) (fpga-manager--entry-locked-p entry))
         (message "%s is already locked" env))
        ((and (= lock-value 0) (not (fpga-manager--entry-locked-p entry)))
         (message "%s is not locked" env))
        (t
         (message "%s %s..."
                  (if (= lock-value 1) "Locking" "Unlocking") env)
         (fpga-manager--run-lock-script (list "-p" env "-l" (number-to-string lock-value))
                                        fpga-manager-sync-before-mutate
                                        status-buffer)))))))

(defun fpga-manager-lock ()
  "Lock the FPGA at point."
  (interactive)
  (fpga-manager--set-lock-state 1))

(defun fpga-manager-unlock ()
  "Unlock the FPGA at point."
  (interactive)
  (fpga-manager--set-lock-state 0))

(defun fpga-manager-force-unlock ()
  "Force unlock the FPGA at point."
  (interactive)
  (fpga-manager--set-lock-state 2 'force))

(defun fpga-manager-refresh ()
  "Refresh the FPGA lock table."
  (interactive)
  (let ((inhibit-read-only t))
    (message "Fetching FPGA status...")
    (let* ((default-directory (fpga-manager--project-fpga-dir))
           (output (shell-command-to-string (fpga-manager--lock-script-command "")))
           (parsed (fpga-manager--parse-output output))
           (built (fpga-manager--build-display-entries parsed)))
      (setq fpga-manager--entries (car built)
            tabulated-list-entries (cdr built)
            fpga-manager--highlight-line nil)
      (tabulated-list-print t)
      (message "Refreshed %d hosts (%d in use) across %d sections."
               (length fpga-manager--entries)
               (fpga-manager--count-locked-hosts fpga-manager--entries)
               (length (cdr (assoc 'sections parsed)))))))

(defun fpga-manager-set-user ()
  "Set the current user for \\='My FPGAs\\=' filtering."
  (interactive)
  (setq fpga-manager-current-user
        (string-trim (read-string "Enter your username (as shown in FPGA table): "
                                  fpga-manager-current-user)))
  (message "Current user set to: %s" fpga-manager-current-user)
  (fpga-manager-refresh))

;;; Test argument parsing

(defun fpga-manager--parse-test-args (args)
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
       ((string-prefix-p "--protocol=" arg) (setq protocol (substring arg 11)))
       ((string-prefix-p "--test-ids=" arg) (setq test-ids (substring arg 11)))
       ((string-prefix-p "--browser=" arg) (setq browser (substring arg 10)))
       ((string-prefix-p "-r " arg) (setq repeat (substring arg 3)))
       ((string-prefix-p "-i " arg) (setq test-ids (substring arg 3)))
       ((string= arg "-d") (setq debug t))
       ((string= arg "--headless") (setq headless t))
       ((string= arg "--keep-locked") (setq keep-locked t))
       (t (push arg remaining))))
    `((protocol . ,protocol)
      (test-ids . ,test-ids)
      (browser . ,browser)
      (repeat . ,repeat)
      (debug . ,debug)
      (headless . ,headless)
      (keep-locked . ,keep-locked)
      (remaining . ,(nreverse remaining)))))

(defun fpga-manager--convert-bitstream-args (args)
  (mapcar (lambda (arg)
            (if (string-prefix-p "-i " arg)
                (concat "--local " (substring arg 3))
              arg))
          (fpga-manager--ensure-list args)))

(defun fpga-manager--build-webapp-command-parts (script-name env parsed-args)
  (let* ((protocol (alist-get 'protocol parsed-args))
         (parts (list script-name protocol env
                      (or (alist-get 'test-ids parsed-args) "webapp"))))
    (unless protocol (error "Protocol is required for webapp tests"))
    (when-let* ((remaining (alist-get 'remaining parsed-args)))
      (setq parts (append parts remaining)))
    (when (alist-get 'debug parsed-args) (setq parts (append parts '("-d"))))
    (when-let* ((repeat (alist-get 'repeat parsed-args)))
      (setq parts (append parts (list "--repeat" repeat))))
    (when-let* ((browser (alist-get 'browser parsed-args)))
      (setq parts (append parts (list "--browser" browser))))
    parts))

(defun fpga-manager--build-webapp-main-command (script-path env parsed-args)
  (fpga-manager--build-uv-run-command
   (fpga-manager--build-webapp-command-parts
    (file-name-nondirectory script-path) env parsed-args)
   (when (alist-get 'headless parsed-args)
     '("env" "-u" "XDG_CURRENT_DESKTOP"))))

(defun fpga-manager-run-test (&optional args)
  "Run test (bitstream or webapp) with ARGS."
  (interactive (list (transient-args 'fpga-manager-test-menu)))
  (fpga-manager--remember-test-args fpga-manager-test-type args)
  (if (eq fpga-manager-test-type 'bitstream)
      (fpga-manager-run-bitstream-test args)
    (fpga-manager-run-webapp-test args)))

(defun fpga-manager-run-bitstream-test (args)
  "Run bitstream toplevel.py test with ARGS for the CLI at point."
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let* ((status-buffer (current-buffer))
            (script-path (fpga-manager--find-script "bitstreams" "toplevel.py"))
            (default-directory (file-name-directory script-path))
            (cmd (fpga-manager--maybe-with-uv-sync
                  fpga-manager-sync-before-mutate
                  (fpga-manager--build-uv-run-command
                   (append (list (file-name-nondirectory script-path) "-p" env)
                           (fpga-manager--flatten-command-args
                            (fpga-manager--convert-bitstream-args args)))))))
       (fpga-manager--bstt-sync-from-bitstream env args)
       (fpga-manager--with-compilation-buffer fpga-manager--test-output-buffer
                                              (lambda ()
                                                (message "Running bitstream test: %s" cmd)
                                                (compile cmd)
                                                (run-with-timer 2 nil #'fpga-manager--refresh-buffer
                                                                status-buffer)))))))

(defun fpga-manager-run-webapp-test (args)
  "Run webapp main.py test with ARGS."
  (fpga-manager--with-env-at-point
   (lambda (env)
     (let* ((script-path (fpga-manager--find-script "webapp" "main.py"))
            (default-directory (file-name-directory script-path))
            (parsed (fpga-manager--parse-test-args args))
            (cmd (fpga-manager--build-webapp-session-command
                  env
                  (fpga-manager--build-webapp-main-command script-path env parsed)
                  (alist-get 'keep-locked parsed))))
       (fpga-manager--bstt-sync-from-webapp env args parsed)
       (fpga-manager--with-compilation-buffer fpga-manager--test-output-buffer
                                              (lambda ()
                                                (message "Running webapp test%s%s: %s"
                                                         (if (alist-get 'headless parsed) " (headless)" "")
                                                         (if (alist-get 'keep-locked parsed) " (keeping host locked)" "")
                                                         cmd)
                                                (compile cmd)))))))

(defun fpga-manager-toggle-test-type ()
  "Toggle between bitstream and webapp test types."
  (interactive)
  (setq fpga-manager-test-type
        (if (eq fpga-manager-test-type 'bitstream) 'webapp 'bitstream))
  (message "Test type: %s" fpga-manager-test-type)
  (transient-setup 'fpga-manager-test-menu))

;;; Transient menus

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

(transient-define-prefix fpga-manager-test-menu ()
  "Transient menu for running FPGA tests."
  :value (lambda ()
           (or fpga-manager-test-last-args
               (if (eq fpga-manager-test-type 'webapp)
                   (fpga-manager--bstt-build-webapp-args)
                 (fpga-manager--bstt-build-bitstream-args))))
  [:description
   (lambda ()
     (if-let* ((env (fpga-manager--get-env-at-point)))
         (format "FPGA Test Menu - CLI: %s" (propertize env 'face 'success))
       (format "FPGA Test Menu - %s"
               (propertize "No CLI selected" 'face 'warning))))
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
     :reader (lambda (prompt _initial _history)
               (fpga-manager--state-read-string-into prompt :repeat)))
    ("-I" "Test ID" "-i "
     :class transient-option
     :prompt "Test ID: "
     :always-read t
     :reader (lambda (prompt _initial _history)
               (fpga-manager--state-read-string-into prompt :test-case)))]
   ["Bitstream Options"
    :if (lambda () (eq fpga-manager-test-type 'bitstream))
    ("-d" "Debug mode" "-d")
    ("-a" "Admin mode (force restricted CLI)" "-a")
    ("-l" "Log level" "-l "
     :class transient-option
     :prompt "Level: "
     :choices ("20" "50")
     :reader (lambda (prompt _initial _history)
               (fpga-manager--state-read-choice-into prompt '("20" "50") :log-level)))
    ("-c" "Custom args" "-c "
     :class transient-option
     :prompt "Custom (key:value): "
     :reader (lambda (prompt _initial _history)
               (fpga-manager--state-read-string-into prompt :custom)))]
   ["Webapp Options"
    :if (lambda () (eq fpga-manager-test-type 'webapp))
    ("-h" "Headless mode" "--headless")
    ("-k" "Keep host locked" "--keep-locked")
    ("-p" "Protocol" "--protocol="
     :class transient-option
     :prompt "Protocol (http/https): "
     :choices ("http" "https")
     :reader (lambda (prompt _initial _history)
               (fpga-manager--state-read-choice-into prompt '("http" "https") :protocol)))
    ("-b" "Browser" "--browser="
     :class transient-option
     :prompt "Browser: "
     :choices ("chrome" "firefox" "safari" "edge")
     :reader (lambda (prompt _initial _history)
               (fpga-manager--state-read-choice-into
                prompt '("chrome" "firefox" "safari" "edge") :webapp-browser)))]]
  [["Actions"
    ("RET" "Run test" fpga-manager-run-test)
    ("q" "Quit" transient-quit-one)]])

;;; Major mode

(defun fpga-manager--highlight-current-row ()
  (when (derived-mode-p 'fpga-manager-mode)
    (let ((line (line-number-at-pos)))
      (unless (eql line fpga-manager--highlight-line)
        (setq fpga-manager--highlight-line line)
        (if (fpga-manager--get-env-at-point)
            (progn
              (unless fpga-manager--current-row-overlay
                (setq fpga-manager--current-row-overlay
                      (make-overlay (point-min) (point-min)))
                (overlay-put fpga-manager--current-row-overlay 'face 'fpga-manager-highlight)
                (overlay-put fpga-manager--current-row-overlay 'priority 1000))
              (move-overlay fpga-manager--current-row-overlay
                            (line-beginning-position) (line-end-position)))
          (when fpga-manager--current-row-overlay
            (delete-overlay fpga-manager--current-row-overlay)
            (setq fpga-manager--current-row-overlay nil)))))))

(defun fpga-manager--print-entry (id cols)
  (if (string-prefix-p "SECTION:" id)
      (let ((title (aref cols 0)))
        (cond
         ((string-match-p "^─+$" title)
          (insert (propertize title 'face 'fpga-manager-separator) "\n"))
         ((not (string-empty-p title))
          (insert (propertize title 'face 'fpga-manager-section-header) "\n"))))
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
         ("User" 28 t)
         ("Date" 16 t)
         ("Reserved" 32 t)
         ("Motherboard" 26 t)
         ("Cards" 6 t)
         ("Model" 10 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (setq-local tabulated-list-printer #'fpga-manager--print-entry)
  (add-hook 'tabulated-list-revert-hook #'fpga-manager-refresh nil t)
  (add-hook 'post-command-hook #'fpga-manager--highlight-current-row nil t)
  (unless fpga-manager-current-user
    (setq fpga-manager-current-user (or (getenv "USER") (user-login-name))))
  (tabulated-list-init-header))

(defun fpga-manager--buffer-name ()
  (if-let* ((proj (project-current))
            (name (file-name-nondirectory
                   (directory-file-name (project-root proj)))))
      (format "*FPGA Manager: %s*" name)
    "*FPGA Manager*"))

;;;###autoload
(defun fpga-manager-status ()
  "Display FPGA Manager status in an interactive buffer."
  (interactive)
  (let ((buffer (get-buffer-create (fpga-manager--buffer-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'fpga-manager-mode)
        (fpga-manager-mode)))
    (switch-to-buffer buffer)
    (fpga-manager-refresh)
    (message "Press '?' for help")))

;;; BSTT commands

(defun bstt/lock-set-port ()
  (interactive)
  (fpga-manager--state-read-string-into "Port (-p, blank to omit): " :port))

(defun bstt/lock-set-value ()
  (interactive)
  (fpga-manager--state-read-string-into "Lock (-l, blank to omit): " :lock-status))

(defun bstt/lock-build-cmd ()
  (let ((args nil)
        (port (fpga-manager--state-get-string :port))
        (lock (fpga-manager--state-get-string :lock-status)))
    (when port (setq args (append args (list "-p" port))))
    (when lock (setq args (append args (list "-l" lock))))
    (fpga-manager--lock-script-command args fpga-manager-sync-before-mutate)))

(defun bstt/lock-run ()
  (interactive)
  (fpga-manager--confirm-and-compile (fpga-manager--project-fpga-dir) (bstt/lock-build-cmd)))

(defun bstt/webapp-compile-set-cli-code ()
  (interactive)
  (fpga-manager--bstt-edit-field 'webapp "CLI Code: " :port))

(defun bstt/webapp-compile-set-batch-code ()
  (interactive)
  (fpga-manager--bstt-edit-field 'webapp "Batch Code: " :test-case))

(defun bstt/webapp-compile-set-browser ()
  (interactive)
  (fpga-manager--bstt-edit-field
   'webapp "Browser: " :webapp-browser '("chrome" "firefox" "safari" "edge")))

(defun bstt/webapp-compile-set-repeat ()
  (interactive)
  (fpga-manager--bstt-edit-field 'webapp "Repeat count: " :repeat))

(defun bstt/webapp-compile-build-cmd ()
  (let* ((env (fpga-manager--state-get :port))
         (parsed (fpga-manager--bstt-webapp-parsed-args))
         (script-path (fpga-manager--find-script "webapp" "main.py")))
    (fpga-manager--build-webapp-session-command
     env
     (fpga-manager--build-webapp-main-command script-path env parsed)
     (alist-get 'keep-locked parsed))))

(defun bstt/webapp-compile-run ()
  (interactive)
  (let* ((script-path (fpga-manager--find-script "webapp" "main.py"))
         (default-directory (file-name-directory script-path)))
    (fpga-manager--bstt-sync-test-menu 'webapp)
    (fpga-manager--confirm-and-compile default-directory (bstt/webapp-compile-build-cmd))))

(defun bstt/toplevel-set-cli-code ()
  (interactive)
  (fpga-manager--bstt-edit-field 'bitstream "CLI Code (-p): " :port))

(defun bstt/toplevel-set-local-code ()
  (interactive)
  (fpga-manager--bstt-edit-field 'bitstream "Local Code (--local): " :test-case))

(defun bstt/toplevel-set-config ()
  (interactive)
  (fpga-manager--bstt-edit-field 'bitstream "Config (-c, blank to omit): " :custom))

(defun bstt/toplevel-set-repeat ()
  (interactive)
  (fpga-manager--bstt-edit-field 'bitstream "Repeat (-r): " :repeat))

(defun bstt/toplevel-build-cmd ()
  (let ((args (list "toplevel.py" "-d" "-p" (fpga-manager--state-get :port)
                    "--local" (fpga-manager--state-get :test-case)
                    "-r" (fpga-manager--state-get :repeat))))
    (when-let* ((cfg (fpga-manager--state-get-string :custom)))
      (setq args (append args (list "-c" cfg))))
    (fpga-manager--maybe-with-uv-sync
     fpga-manager-sync-before-mutate
     (fpga-manager--build-uv-run-command args))))

(defun bstt/toplevel-run ()
  (interactive)
  (let* ((script-path (fpga-manager--find-script "bitstreams" "toplevel.py"))
         (default-directory (file-name-directory script-path)))
    (fpga-manager--bstt-sync-test-menu 'bitstream)
    (fpga-manager--confirm-and-compile default-directory (bstt/toplevel-build-cmd))))

(defun bstt/code-check-run ()
  (interactive)
  (let* ((script-path (fpga-manager--find-script "webapp" "main.py"))
         (default-directory (file-name-directory script-path))
         (cmd (fpga-manager--maybe-with-uv-sync
               fpga-manager-sync-before-mutate
               (fpga-manager--build-uv-run-command
                (list "code_submission_check.py")))))
    (fpga-manager--confirm-and-compile default-directory cmd)))

(provide 'fpga-manager)
;;; fpga-manager.el ends here
