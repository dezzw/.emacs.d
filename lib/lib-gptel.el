;;; lib-gptel.el --- gptel helpers -*- lexical-binding: t -*-
;;; Commentary:
;; Helper functions for gptel, including dynamic model fetching.
;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)

(defvar +gptel-netint-host "cursor.netint.ca/openrouter/v1"
  "Host for the NETINT OpenRouter-compatible API.")

(defun +gptel-fetch-models ()
  "Fetch available models from the NETINT endpoint and return as a list."
  (let* ((api-key (auth-source-pick-first-password
                   :host "cursor.netint.ca" :user "netint"))
         (url (format "https://%s/models" +gptel-netint-host))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " api-key))
            ("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (response (json-read))
             (data (plist-get response :data)))
        (mapcar (lambda (model) (plist-get model :id)) data)))))

(defun +gptel-refresh-models ()
  "Refresh the model list for the NETINT gptel backend."
  (interactive)
  (require 'gptel)
  (condition-case err
      (let ((models (+gptel-fetch-models)))
        (when (and gptel-backend models)
          (setf (gptel-backend-models gptel-backend) models)
          (message "gptel: refreshed %d models" (length models))))
    (error (message "gptel: failed to fetch models: %s" err))))

(defun +gptel-make-netint-backend ()
  "Create and return the NETINT gptel backend with fetched models."
  (require 'gptel)
  (gptel-make-openai "NETINT"
    :host +gptel-netint-host
    :endpoint "/chat/completions"
    :stream t
    :key (auth-source-pick-first-password :host "cursor.netint.ca" :user "netint")
    :models (condition-case nil
                (+gptel-fetch-models)
              (error '("openrouter/auto")))))

(provide 'lib-gptel)
;;; lib-gptel.el ends here
