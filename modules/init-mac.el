;;; init-mac.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Make mouse wheel / trackpad scrolling less jerky
(setup mwheel
  (:option mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))))

(setup (:with-feature nil)
  (keymap-global-set "<wheel-right>" 'ignore)
  (keymap-global-set "<wheel-left>" 'ignore)
  (keymap-global-set "<double-wheel-right>" 'ignore)
  (keymap-global-set "<double-wheel-left>" 'ignore)
  (keymap-global-set "<triple-wheel-right>" 'ignore)
  (keymap-global-set "<triple-wheel-left>" 'ignore)
  (keymap-global-set "M-`" 'ns-next-frame))

(setup (:only-if (and (display-graphic-p)))
   (:require lib-env)
   (+load-env-file))

(setup emt
  (:defer (:require emt))
  (:when-loaded
    (keymap-global-set "M-f" 'emt-forward-word)
    (keymap-global-set "M-b" 'emt-backward-word)
    (emt-ensure)))

(provide 'init-mac)
;;; init-mac.el ends here
