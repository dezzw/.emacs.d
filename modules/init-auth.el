;;; init-auth.el --- Authentication and secrets -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup auth-source
  (setopt auth-sources '("~/.authinfo" macos-keychain-generic macos-keychain-internet)))

(provide 'init-auth)
;;; init-auth.el ends here
