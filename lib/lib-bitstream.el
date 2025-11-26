;; lib-bitstream.el --- Bitstream-related configurations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; BSTT Lock Command Helper Functions
(defvar bstt/lock-port "cli455")
(defvar bstt/lock-value "")

(defun bstt/lock-set-port ()
  (interactive)
  (setq bstt/lock-port (read-string "Port (-p, blank to omit): " bstt/lock-port)))

(defun bstt/lock-set-value ()
  (interactive)
  (setq bstt/lock-value (read-string "Lock (-l, blank to omit): " bstt/lock-value)))

(defun bstt/lock-build-cmd ()
  (let ((args (list "/home/desmond/workspace/bitstream/test/fpga/linuxPC_Lock.py")))
    (when (and bstt/lock-port (not (string-empty-p bstt/lock-port)))
      (setq args (append args (list "-p" bstt/lock-port))))
    (when (and bstt/lock-value (not (string-empty-p bstt/lock-value)))
      (setq args (append args (list "-l" bstt/lock-value))))
    (format "python3 %s" (mapconcat #'shell-quote-argument args " "))))

(defun bstt/lock-run ()
  (interactive)
  (let* ((cmd (bstt/lock-build-cmd))
         (project-root (project-root (project-current t)))
         (default-directory (expand-file-name "webapp" project-root))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s_compilation*"
                    (file-name-nondirectory (directory-file-name project-root))))))
    (when (yes-or-no-p (format "Run command in %s: %s ?" default-directory cmd))
      (compile cmd))))

;; BSTT Webapp Compile Command Helper Functions
(defvar bstt/webapp-cli-code "CLI109")
(defvar bstt/webapp-batch-code "B000006")
(defvar bstt/webapp-browser "chrome")
(defvar bstt/webapp-repeat "1")

(defun bstt/webapp-compile-set-cli-code ()
  (interactive)
  (setq bstt/webapp-cli-code (read-string "CLI Code: " bstt/webapp-cli-code)))

(defun bstt/webapp-compile-set-batch-code ()
  (interactive)
  (setq bstt/webapp-batch-code (read-string "Batch Code: " bstt/webapp-batch-code)))

(defun bstt/webapp-compile-set-browser ()
  (interactive)
  (setq bstt/webapp-browser (completing-read "Browser: "
                                            '("chrome" "firefox" "safari" "edge")
                                            nil t bstt/webapp-browser)))

(defun bstt/webapp-compile-set-repeat ()
  (interactive)
  (setq bstt/webapp-repeat (read-string "Repeat count: " bstt/webapp-repeat)))

(defun bstt/webapp-compile-build-cmd ()
  (format "uv run main.py http %s %s --browser %s --repeat %s"
          bstt/webapp-cli-code bstt/webapp-batch-code bstt/webapp-browser bstt/webapp-repeat))

(defun bstt/webapp-compile-run ()
  (interactive)
  (let* ((cmd (bstt/webapp-compile-build-cmd))
         (project-root (project-root (project-current t)))
         (default-directory (expand-file-name "webapp" project-root))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s_compilation*"
                    (file-name-nondirectory (directory-file-name project-root))))))
    (when (yes-or-no-p (format "Run command in %s: %s ?" default-directory cmd))
      (compile cmd))))

;; BSTT Toplevel Command Helper Functions
(defvar bstt/toplevel-cli-code "CLI455")
(defvar bstt/toplevel-local-code "B000006")
(defvar bstt/toplevel-config "")
(defvar bstt/toplevel-repeat "1")

(defun bstt/toplevel-set-cli-code ()
  (interactive)
  (setq bstt/toplevel-cli-code (read-string "CLI Code (-p): " bstt/toplevel-cli-code)))

(defun bstt/toplevel-set-local-code ()
  (interactive)
  (setq bstt/toplevel-local-code (read-string "Local Code (--local): " bstt/toplevel-local-code)))

(defun bstt/toplevel-set-config ()
  (interactive)
  (setq bstt/toplevel-config (read-string "Config (-c, blank to omit): " bstt/toplevel-config)))

(defun bstt/toplevel-set-repeat ()
  (interactive)
  (setq bstt/toplevel-repeat (read-string "Repeat (-r): " bstt/toplevel-repeat)))

(defun bstt/toplevel-build-cmd ()
  (let ((args (list "uv" "run" "toplevel.py" "-d" "-p" bstt/toplevel-cli-code "--local" bstt/toplevel-local-code "-r" bstt/toplevel-repeat)))
    (when (and bstt/toplevel-config (not (string-empty-p bstt/toplevel-config)))
      (setq args (append args (list "-c" bstt/toplevel-config))))
    (mapconcat #'shell-quote-argument args " ")))

(defun bstt/toplevel-run ()
  (interactive)
  (let* ((cmd (bstt/toplevel-build-cmd))
         (project-root (project-root (project-current t)))
         (default-directory (expand-file-name "bitstreams" project-root))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s_compilation*"
                    (file-name-nondirectory (directory-file-name project-root))))))
    (when (yes-or-no-p (format "Run command in %s: %s ?" default-directory cmd))
      (compile cmd))))

;; BSTT Code Submission Check Command Helper Functions
(defun bstt/code-check-run ()
  (interactive)
  (let* ((cmd "uv run code_submission_check.py")
         (project-root (project-root (project-current t)))
         (default-directory (expand-file-name "webapp" project-root))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*%s_compilation*"
                    (file-name-nondirectory (directory-file-name project-root))))))
    (when (yes-or-no-p (format "Run command in %s: %s ?" default-directory cmd))
      (compile cmd))))

;;;; provide
(provide 'lib-bitstream)
;;; lib-bitstream.el ends here.
