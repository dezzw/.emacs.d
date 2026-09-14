;;; lib-panel.el --- panel startup tweaks -*- lexical-binding: t -*-
;;; Commentary:
;; Panel uses `(raise …)` display properties that trigger spurious
;; face-attribute redisplay errors on Emacs 32.  Replace the helpers
;; that set them with plain face properties.
;;; Code:

(declare-function panel--insert-text "panel" (text))
(declare-function panel--with-icon-fallback "panel" (fn icon fallback &rest args))
(declare-function panel--weather-info-p "panel" ())
(declare-function panel--nerd-icons-available-p "panel" ())

(defvar panel--weather-description)
(defvar panel--weather-icon)
(defvar panel--weather-temperature)
(defvar panel--weather-error-message)
(defvar panel--temperature)

(defun +panel-setup ()
  "Use Emacs-32-safe panel render helpers (no `raise' display specs)."
  (when (fboundp 'panel--insert-startup-time)
    (defalias 'panel--insert-startup-time #'+panel--insert-startup-time)
    (defalias 'panel--insert-package-info #'+panel--insert-package-info)
    (defalias 'panel--insert-weather-info #'+panel--insert-weather-info)))

(defun +panel--insert-startup-time ()
  (panel--insert-text
   (format "%s %s %s %s"
           (panel--with-icon-fallback #'nerd-icons-octicon "nf-oct-clock" "time")
           (propertize "Startup time:" 'face 'panel-text-info-face)
           (propertize (emacs-init-time "%.2f") 'face 'panel-startup-time-face)
           (propertize "seconds" 'face 'panel-text-info-face))))

(defun +panel--insert-package-info (packages)
  (panel--insert-text
   (format "%s %s %s"
           (panel--with-icon-fallback #'nerd-icons-codicon "nf-cod-package" "pkg")
           (propertize packages 'face 'panel-info-face)
           (propertize "packages loaded" 'face 'panel-text-info-face))))

(defun +panel--insert-weather-info ()
  (when (panel--weather-info-p)
    (let ((beg (point))
          (icon (or panel--weather-icon "")))
      (if panel--weather-description
          (panel--insert-text
           (if (string-empty-p icon)
               (format "%s, %s%s"
                       (propertize panel--weather-description
                                   'face 'panel-weather-description-face)
                       (propertize panel--temperature
                                   'face 'panel-weather-temperature-face)
                       (propertize "℃" 'face 'panel-text-info-face))
             (format "%s %s, %s%s"
                     (if (panel--nerd-icons-available-p)
                         icon
                       (propertize icon 'face 'panel-weather-icon-face))
                     (propertize panel--weather-description
                                 'face 'panel-weather-description-face)
                     (propertize panel--temperature
                                 'face 'panel-weather-temperature-face)
                     (propertize "℃" 'face 'panel-text-info-face))))
        (panel--insert-text
         (propertize (or panel--weather-error-message "Loading weather data...")
                     'face 'panel-weather-temperature-face)))
      (put-text-property beg (point) 'panel-section 'weather))))

(provide 'lib-panel)
;;; lib-panel.el ends here
