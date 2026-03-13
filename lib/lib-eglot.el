;;; lib-eglot.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'eglot)
(require 'flymake)

;; https://github.com/joaotavora/eglot/issues/1296
;; related to (setq flymake-no-changes-timeout nil)
;; `rassumfrassum` streams diagnostics with `$/streamDiagnostics`, so cover
;; both that extension and the standard `publishDiagnostics` notification.
(defun +eglot-restart-flymake-on-diagnostics (uri)
  "Restart Flymake for the visiting buffer associated with URI."
  (when-let* ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer buffer
      (if (and (eq nil flymake-no-changes-timeout)
               (not (buffer-modified-p)))
          (flymake-start t)))))

(defun +eglot-report-streamed-diagnostics ()
  "Flush cached streamed diagnostics into Flymake."
  (when (and eglot--flymake-report-fn eglot--streamed-diagnostics)
    (let ((version (car eglot--streamed-diagnostics))
          (map (cadr eglot--streamed-diagnostics)))
      (if (and version (< version eglot--docver))
          (eglot--flymake-report-2 nil :stay)
        (cl-loop for entry in map
                 for mode = :clear then :stay
                 do (eglot--flymake-report-1 (cdr entry) mode :force t))))))

(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql textDocument/publishDiagnostics)) &key uri
           &allow-other-keys)
  (+eglot-restart-flymake-on-diagnostics uri))

(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql $/streamDiagnostics)) &key uri
           &allow-other-keys)
  (when-let* ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer buffer
      (+eglot-report-streamed-diagnostics))))

(defun vue-eglot-init-options ()
  "VUE language server init options."
  (let ((tsdk-path (expand-file-name "typescript/lib"
                                     (string-trim-right (shell-command-to-string "npm root -g")))))
    `(:typescript (:tsdk ,tsdk-path
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t))
                  :vue (:hybridMode :json-false))))

(provide 'lib-eglot)
;;; lib-eglot.el ends here
