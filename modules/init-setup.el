;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary:

;; setup extension

;;; Code:

(straight-use-package 'setup)
(require 'setup)

(setup-define :pkg
  (lambda (pkg-or-pkgs)
    (let ((pkgs (if (and (listp pkg-or-pkgs)
                         ;; Check if it's a single package with recipe
                         (not (and (symbolp (car pkg-or-pkgs))
                                   (keywordp (cadr pkg-or-pkgs)))))
                    pkg-or-pkgs
                  (list pkg-or-pkgs))))
      `(progn
         ,@(mapcar (lambda (pkg)
                     (cond
                      ;; Format: (pkg-name :key val :key val ...)
                      ((and (listp pkg) (symbolp (car pkg)) (keywordp (cadr pkg)))
                       (let ((pkg-name (car pkg))
                             (recipe (cdr pkg)))
                         `(progn
                            (unless (featurep ',pkg-name)
                              (straight-use-package ',(cons pkg-name recipe)))
                            (require ',pkg-name))))
                      ;; Format: pkg-name (simple symbol)
                      ((symbolp pkg)
                       `(progn
                          (unless (featurep ',pkg)
                            (straight-use-package ',pkg))
                          (require ',pkg)))
                      ;; Fallback
                      (t
                       `(progn
                          (unless (featurep ',(car pkg))
                            (straight-use-package ',pkg))
                          (require ',(car pkg))))))
                   pkgs))))
  :documentation "Use straight to install and require PKG-OR-PKGS.
Can accept:
- A single package: pkg-name
- A single package with recipe: (pkg-name :key val ...)
- A list of any combination of the above"
  :repeatable t)

(setup-define :defer
  (lambda (features)
    `(run-with-idle-timer 1 nil
                          (lambda ()
                            ,features)))
  :documentation "Delay loading the feature until a certain amount of idle time has passed."
  :repeatable t)

(setup-define :advice
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :set-font
  (lambda (font)
    `(add-hook ',(setup-get 'hook)
               (lambda ()
                 (let ((face-name (intern (format "%s-font-face" ',(setup-get 'mode)))))
                   (unless (facep face-name)
                     (make-face face-name))
                   (set-face-attribute face-name nil :font ,font)
                   (setq buffer-face-mode-face face-name)
                   (buffer-face-mode)))))
  :documentation "Set the font for the current mode.
This will create a unique face for the mode and set the buffer
font to FONT using that face."
  :debug '(form)
  :repeatable t)

(provide 'init-setup)
;;; init-setup.el ends here
