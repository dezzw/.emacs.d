;;; lib-savehist.el --- Safe savehist startup -*- lexical-binding: t -*-

(defun +savehist--rotate-file ()
  (when (file-exists-p savehist-file)
    (rename-file savehist-file
                 (make-temp-name (concat savehist-file ".bak."))
                 nil)))

(defun +savehist-safe-start ()
  "Enable `savehist-mode', recovering from oversized or corrupt history."
  (when (let ((attrs (file-attributes savehist-file)))
          (and attrs (> (file-attribute-size attrs) (* 4 1024 1024))))
    (+savehist--rotate-file))
  (condition-case _
      (savehist-mode 1)
    (error
     (+savehist--rotate-file)
     (savehist-mode 1))))

(provide 'lib-savehist)
