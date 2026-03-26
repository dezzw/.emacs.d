;;; init-mac.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Make mouse wheel / trackpad scrolling less jerky
(setup mwheel
  (:option mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))))

(setup (:with-feature nil)
  (:global-bind "<wheel-right>" 'ignore
                "<wheel-left>" 'ignore
                "<double-wheel-right>" 'ignore
                "<double-wheel-left>" 'ignore
                "<triple-wheel-right>" 'ignore
                "<triple-wheel-left>" 'ignore
                "M-`" 'ns-next-frame))

(setup (:only-if (and (display-graphic-p)))
  (:require lib-env)
  (+load-env-file))

(setup emt
  (:defer (:require emt))
  (:when-loaded
    (:global-bind "M-f" 'emt-forward-word
                  "M-b" 'emt-backward-word)
    (emt-ensure)))

;; Open files with macOS default browser
(with-eval-after-load 'org
  (setcdr (assq t org-file-apps-gnu) 'browse-url-default-macosx-browser))

(provide 'init-mac)
;;; init-mac.el ends here
