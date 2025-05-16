;;; init-auth.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup auth-sources
  (:option
   auth-sources '(macos-keychain-generic macos-keychain-internet "~/.authinfo")))


(provide 'init-auth)
;;; init-auth.el ends here
