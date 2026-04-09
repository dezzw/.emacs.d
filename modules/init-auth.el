;;; init-auth.el --- Authentication and secrets -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup auth-source
  (:option
   auth-sources '(macos-keychain-generic macos-keychain-internet "~/.authinfo")))

(provide 'init-auth)
;;; init-auth.el ends here
