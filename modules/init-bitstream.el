;;; init-bitstream.el --- Bitstream-related configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +jenkins-api-token ()
  "Return the Jenkins API token from `auth-source'."
  (auth-source-pick-first-password
   :host "jenkins.netint.ca"
   :user "desmond.wang"
   :port 8443))

(setup jenkins
  (:option jenkins-url "https://jenkins.netint.ca:8443/"
           jenkins-username "desmond.wang")
  (:when-loaded
    (setq jenkins-api-token (+jenkins-api-token))))

(setup transient
  (:require transient)
  (:when-loaded
    (:also-load fpga-manager)
    (:global-bind "C-c e f" 'fpga-manager-status
                  "C-c e b" 'fpga-manager-menu
                  "C-c e l" 'bstt/lock
                  "C-c e w" 'bstt/webapp-compile
                  "C-c e t" 'bstt/toplevel
                  "C-c e c" 'bstt/code-check)

    (transient-define-prefix bstt/lock ()
      "Edit args and run linuxPC_Lock.py in project_root/fpga directory."
      :info-manual "(bstt-lock) BSTT Lock Command"
      [ ["Arguments (press to modify, menu stays open)"
         ("p" (lambda () (format "Port: %s" (fpga-manager-state-get :port)))
          bstt/lock-set-port :transient t)
         ("l" (lambda () (format "Lock: %s" (fpga-manager-state-get :lock-status)))
          bstt/lock-set-value :transient t)]
        ["Actions"
         ("RET" "Run command" bstt/lock-run)
         ("q" "Quit menu" transient-quit-one)] ])

    (transient-define-prefix bstt/webapp-compile ()
      "Edit args and run webapp compile command in project_root/webapp directory."
      :info-manual "(bstt-webapp-compile) BSTT Webapp Compile Command"
      [ ["Arguments (press to modify, menu stays open)"
         ("c" (lambda () (format "CLI Code: %s" (fpga-manager-state-get :port)))
          bstt/webapp-compile-set-cli-code :transient t)
         ("b" (lambda () (format "Batch Code: %s" (fpga-manager-state-get :test-case)))
          bstt/webapp-compile-set-batch-code :transient t)
         ("w" (lambda () (format "Browser: %s" (fpga-manager-state-get :webapp-browser)))
          bstt/webapp-compile-set-browser :transient t)
         ("r" (lambda () (format "Repeat: %s" (fpga-manager-state-get :repeat)))
          bstt/webapp-compile-set-repeat :transient t)]
        ["Actions"
         ("RET" "Run command" bstt/webapp-compile-run)
         ("q" "Quit menu" transient-quit-one)] ])

    (transient-define-prefix bstt/toplevel ()
      "Edit args and run toplevel.py in project_root/bitstreams directory."
      :info-manual "(bstt-toplevel) BSTT Toplevel Command"
      [ ["Arguments (press to modify, menu stays open)"
         ("p" (lambda () (format "CLI Code (-p): %s" (fpga-manager-state-get :port)))
          bstt/toplevel-set-cli-code :transient t)
         ("l" (lambda () (format "Local Code (--local): %s" (fpga-manager-state-get :test-case)))
          bstt/toplevel-set-local-code :transient t)
         ("c" (lambda ()
                (let ((cfg (or (fpga-manager-state-get :custom) "")))
                  (format "Config (-c): %s" (if (string-empty-p cfg) "<none>" cfg))))
          bstt/toplevel-set-config :transient t)
         ("r" (lambda () (format "Repeat (-r): %s" (fpga-manager-state-get :repeat)))
          bstt/toplevel-set-repeat :transient t)]
        ["Actions"
         ("RET" "Run command" bstt/toplevel-run)
         ("q" "Quit menu" transient-quit-one)] ])

    (transient-define-prefix bstt/code-check ()
      "Run code_submission_check.py in project_root/webapp directory."
      :info-manual "(bstt-code-check) BSTT Code Submission Check Command"
      [ ["Actions"
         ("RET" "Run command" bstt/code-check-run)
         ("q" "Quit menu" transient-quit-one)] ])))

(provide 'init-bitstream)
;;; init-bitstream.el ends here
