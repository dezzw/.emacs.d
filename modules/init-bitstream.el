;;; init-bitstream.el --- Bitstream-related configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup jira
  (setq jira-base-url "https://netint.atlassian.net"))

(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load fpga-manager)
    (keymap-global-set "C-c e f" 'fpga-manager-status)
    (keymap-global-set "C-c e m" 'fpga-manager-menu)
    (keymap-global-set "C-c e l" 'bstt/lock)
    (keymap-global-set "C-c e w" 'bstt/webapp-compile)
    (keymap-global-set "C-c e t" 'bstt/toplevel)
    (keymap-global-set "C-c e c" 'bstt/code-check)

    ;; BSTT Lock Command Transient
    (transient-define-prefix bstt/lock ()
      "Edit args and run linuxPC_Lock.py in project_root/fpga directory."
      :info-manual "(bstt-lock) BSTT Lock Command"
      [ ["Arguments (press to modify, menu stays open)"
         ("p" (lambda () (format "Port: %s" bstt/lock-port)) bstt/lock-set-port :transient t)
         ("l" (lambda () (format "Lock: %s" bstt/lock-value)) bstt/lock-set-value :transient t)]
        ["Actions"
         ("RET" "Run command" bstt/lock-run)
         ("q" "Quit menu" transient-quit-one)] ])

    ;; BSTT Webapp Compile Command Transient
    (transient-define-prefix bstt/webapp-compile ()
      "Edit args and run webapp compile command in project_root/webapp directory."
      :info-manual "(bstt-webapp-compile) BSTT Webapp Compile Command"
      [ ["Arguments (press to modify, menu stays open)"
         ("c" (lambda () (format "CLI Code: %s" bstt/webapp-cli-code)) bstt/webapp-compile-set-cli-code :transient t)
         ("b" (lambda () (format "Batch Code: %s" bstt/webapp-batch-code)) bstt/webapp-compile-set-batch-code :transient t)
         ("w" (lambda () (format "Browser: %s" bstt/webapp-browser)) bstt/webapp-compile-set-browser :transient t)
         ("r" (lambda () (format "Repeat: %s" bstt/webapp-repeat)) bstt/webapp-compile-set-repeat :transient t)]
        ["Actions"
         ("RET" "Run command" bstt/webapp-compile-run)
         ("q" "Quit menu" transient-quit-one)] ])

    ;; BSTT Toplevel Command Transient
    (transient-define-prefix bstt/toplevel ()
      "Edit args and run toplevel.py in project_root/bitstreams directory."
      :info-manual "(bstt-toplevel) BSTT Toplevel Command"
      [ ["Arguments (press to modify, menu stays open)"
         ("p" (lambda () (format "CLI Code (-p): %s" bstt/toplevel-cli-code)) bstt/toplevel-set-cli-code :transient t)
         ("l" (lambda () (format "Local Code (--local): %s" bstt/toplevel-local-code)) bstt/toplevel-set-local-code :transient t)
         ("c" (lambda () (format "Config (-c): %s" (if (string-empty-p bstt/toplevel-config) "<none>" bstt/toplevel-config))) bstt/toplevel-set-config :transient t)
         ("r" (lambda () (format "Repeat (-r): %s" bstt/toplevel-repeat)) bstt/toplevel-set-repeat :transient t)]
        ["Actions"
         ("RET" "Run command" bstt/toplevel-run)
         ("q" "Quit menu" transient-quit-one)] ])

    ;; BSTT Code Submission Check Command Transient
    (transient-define-prefix bstt/code-check ()
      "Run code_submission_check.py in project_root/webapp directory."
      :info-manual "(bstt-code-check) BSTT Code Submission Check Command"
      [ ["Actions"
         ("RET" "Run command" bstt/code-check-run)
         ("q" "Quit menu" transient-quit-one)] ])))

(provide 'init-bitstream)
;;; init-bitstream.el ends here
