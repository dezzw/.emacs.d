;;; lib-telega.el --- Telega helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +telega-save-file-to-clipboard (msg)
  "Save file at point to clipboard.
NOTE: macOS only."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file
      :priority 32
      :update-callback
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p dfile)
          (let* ((fpath (telega--tl-get dfile :local :path))
                 (command (if *is-mac*
                              (list "osascript" "-e" (format "set the clipboard to POSIX file \"%s\"" fpath))
                            (list "sh" "-c" (format "wl-copy < \"%s\"" fpath)))))
            (make-process
             :name "telega-clipboard"
             :buffer nil
             :command command
             :sentinel (lambda (process event)
                         (message "Process %s had event %s" process event)))))))))

(defun +telega-msg-save-to-cloud-copyleft (msg)
  "Save messages's MSG media content to a file.
     If MSG is an animation message, then possibly add animation to
     the saved animations list."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file
      :priority 32
      :update-callback
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p dfile)
          ;; TODO: This might be executed in process filter, so
          ;; pressing C-g will trigger "error in process filter: Quit"
          ;; Need to execute this outside of process filter
          (let* ((fpath (telega--tl-get dfile :local :path))
                 (fname (file-name-nondirectory fpath)))
            (telega--sendMessage
             (telega-chat-me)
             (list :@type "inputMessageDocument"
                   :document (telega-chatbuf--gen-input-file
                                 fpath 'Document)
                   :caption (telega-fmt-text "#copyleft")
                   :disable_content_type_detection nil))
            (message (format "Saved to cloud: %s" fname))))))))

(defvar +telega-notification-cache nil
  "Plist of Telega notification counts, or nil when there are none.")

(defun +telega-notification--live-p ()
  "Return non-nil when the Telega server is connected."
  (and (fboundp 'telega-server-live-p)
       (telega-server-live-p)
       (buffer-live-p telega-server--buffer)))

(defun +telega-notification--fetch-counts ()
  "Return a plist of Telega notification counts, or nil when there are none."
  (when (+telega-notification--live-p)
    (let* ((keyword-count (length (ring-elements telega--notification-messages-ring)))
           (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
           (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count)
                                             (telega-filter-chats (telega-chats-list)
                                               '(mention)))))
           (total (+ mentioned-count unread-count keyword-count)))
      (when (> total 0)
        (list :unread-count unread-count
              :mentioned-count mentioned-count
              :keyword-count keyword-count
              :total total)))))

(defun +telega-notification-update (&rest _)
  "Refresh cached Telega notification counts and redraw the mode line.
Used from Telega hooks and advice, so extra args are ignored."
  (let ((new (+telega-notification--fetch-counts)))
    (unless (equal new +telega-notification-cache)
      (setq +telega-notification-cache new)
      (force-mode-line-update))))

(defun +telega-notification-icon ()
  "Return the Telega nerd icon."
  (when (fboundp 'nerd-icons-faicon)
    (nerd-icons-faicon "nf-fae-telegram" :face '(:inherit nerd-icons-purple))))

(defun +mode-line-telega-icon ()
  "Return a detailed Telega notification indicator for the mode line.
Reads `+telega-notification-cache' only; must not fetch during mode-line eval."
  (when-let ((counts +telega-notification-cache))
    (concat (+telega-notification-icon)
            "["
            (when (> (plist-get counts :unread-count) 0)
              (propertize (concat " ●​​​" (number-to-string (plist-get counts :unread-count)))
                          'face 'telega-unmuted-count))
            (when (> (plist-get counts :mentioned-count) 0)
              (propertize (concat " @​​​" (number-to-string (plist-get counts :mentioned-count)))
                          'face 'telega-mention-count))
            (when (> (plist-get counts :keyword-count) 0)
              (propertize (concat " #​​​" (number-to-string (plist-get counts :keyword-count)))
                          'face 'telega-unmuted-count))
            "]")))

(provide 'lib-telega)
;;; lib-telega.el ends here
