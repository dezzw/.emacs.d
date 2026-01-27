;;; lib-activities.el --- Consult + activities buffer isolation  -*- lexical-binding: t -*-
;;; Commentary:
;;; Helpers for activity-scoped buffer behavior: switch-to-buffer and
;;; kill-buffer use only the current activity's buffers when in an activity;
;;; other-buffer is restricted to activity buffers; on resume/switch, windows
;;; showing buffers from other tabs are replaced with *scratch*.
;;; Loaded via (setup activities (:when-loaded (:after consult) (:also-load lib-activities) (+activities-consult-setup))).
;;; Code:

(require 'cl-lib)
(require 'consult)

(defvar consult--source-activities
  `(:name     "Activity Buffers"
              :narrow   ?a
              :category buffer
              :history  buffer-name-history
              :action   ,#'switch-to-buffer
              :enabled  ,(lambda () (activities-current))
              :items
              ,(lambda ()
                 (consult--buffer-query
                  :predicate (lambda (b)
                               (memq b (activities-tabs--tab-parameter
                                        'activities-buffer-list
                                        (activities-tabs--tab (activities-current)))))
                  :sort 'visibility
                  :as #'buffer-name)))
  "Consult source for current activity's buffers.")

(defun +consult-buffer-activities-aware (&optional sources)
  "Call consult-buffer; when in an activity, use only activity buffers."
  (interactive)
  (consult-buffer (or sources
                      (and (bound-and-true-p activities-tabs-mode)
                           (activities-current)
                           (list consult--source-activities)))))

(defun +kill-buffer-activities-aware (&optional buffer-or-name)
  "Kill a buffer; when in an activity, only offer current activity's buffers."
  (interactive
   (list (if (and (bound-and-true-p activities-tabs-mode)
                 (activities-current))
             (progn
               (consult--multi
                (list (plist-put (copy-sequence consult--source-activities)
                                :action (lambda (cand)
                                          (when (get-buffer cand)
                                            (kill-buffer (get-buffer cand))))))
                :prompt "Kill buffer: "
                :require-match t
                :history 'buffer-name-history
                :sort nil)
               nil)
           (read-buffer "Kill buffer: " (current-buffer) t))))
  (when buffer-or-name
    (let ((buf (get-buffer buffer-or-name)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun +activities-other-buffer (orig &rest args)
  "Return an activity buffer when in an activity, else call ORIG."
  (if (and (bound-and-true-p activities-tabs-mode)
           (activities-current))
      (let* ((buffer (or (car args) (current-buffer)))
             (visible-ok (or (cadr args) t))
             (tab (activities-tabs--tab (activities-current)))
             (candidates (activities-tabs--tab-parameter 'activities-buffer-list tab))
             (ok (seq-filter (lambda (b)
                              (and (buffer-live-p b)
                                   (not (eq b buffer))
                                   (or visible-ok (not (get-buffer-window b 'visible)))))
                            candidates)))
        (or (car ok) (apply orig args)))
    (apply orig args)))

(defun +activities-sanitize-windows-from-other-activities (&optional _activity)
  "In current tab, replace any window buffer that belongs to another tab with *scratch*.
So resuming/creating/switching to an activity does not show buffers from other workspaces."
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'tab-bar--current-tab-find)
             (boundp 'tab-bar-tabs-function))
    (let* ((tabs (funcall tab-bar-tabs-function))
           (current-tab (tab-bar--current-tab-find))
           (other-tabs (remove current-tab tabs))
           (other-buffers (seq-filter #'buffer-live-p
                                      (cl-loop for tab in other-tabs
                                               append (copy-sequence
                                                       (alist-get 'activities-buffer-list (cdr tab))))))
           (scratch (get-buffer-create "*scratch*")))
      (when other-buffers
        (walk-windows
         (lambda (win)
           (when (memq (window-buffer win) other-buffers)
             (set-window-buffer win scratch)))
         nil (selected-frame))))))

(defun +activities-consult-setup ()
  "Wire activity-scoped buffer behavior: consult source, remaps, advice, hooks.
Call after both activities and consult are loaded."
  (when (boundp 'consult--source-buffer)
    (setq consult--source-buffer
          (plist-put (plist-put consult--source-buffer
                                :enabled (lambda () (not (activities-current))))
                     :default t)))
  (add-to-list 'consult-buffer-sources 'consult--source-activities)
  (add-hook 'after-init-hook
            (lambda ()
              (keymap-global-set "<remap> <switch-to-buffer>"
                                #'+consult-buffer-activities-aware)
              (keymap-global-set "<remap> <kill-buffer>"
                                #'+kill-buffer-activities-aware))
            0)
  (advice-add #'other-buffer :around #'+activities-other-buffer
              '((name . +activities-other-buffer) :depth 100))
  (add-hook 'activities-after-resume-functions
            #'+activities-sanitize-windows-from-other-activities)
  (add-hook 'activities-after-switch-functions
            #'+activities-sanitize-windows-from-other-activities))

(provide 'lib-activities)
;;; lib-activities.el ends here
