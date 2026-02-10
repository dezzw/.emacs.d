;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary:

;; setup extension

;;; Code:

;; Setup is now installed via Nix, just require it
(require 'setup)

(setup-define :defer
  (lambda (features)
    `(run-with-idle-timer 1 nil
                          (lambda ()
                            (catch 'setup-quit
                              ,features))))
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

(setup-define :pkg
  (lambda (&optional package)
    (let* ((feature (setup-get 'feature))
           ;; Normalize: infer name from feature when needed
           (spec (cond
                  ((null package) nil)                              ; (:pkg)
                  ((symbolp package) package)                       ; (:pkg meow)
                  ((and (consp package) (symbolp (car package)))
                   package)                                         ; (:pkg (meow :url ...))
                  ((and (consp package) (keywordp (car package)))
                   (cons feature package))                          ; (:pkg (:url ...))
                  (t (error "Invalid :pkg argument: %S" package))))
           (is-vc (consp spec))
           (name (if is-vc (car spec) (or spec feature)))
           (vc-plist (and is-vc (cdr spec)))
           (url (and vc-plist (plist-get vc-plist :url))))
      (if url
          `(unless (require ',feature nil t)
             (package-vc-install ',(cons name vc-plist))
             (require ',feature))
        `(require ',(or name feature) nil t))))
  :documentation "Declare package dependency.
Tries `require' first (succeeds when installed via Nix).
With no argument, infers package name from the setup form name.
Falls back to `package-vc-install' when a VC spec is given.

Usage:
  (:pkg)                                    ; infer from (setup NAME ...)
  (:pkg meow)                               ; require 'meow
  (:pkg pdf-tools)                          ; feature != package
  (:pkg (eglot-x :url \"https://...\"))    ; require or vc-install
  (:pkg (:url \"https://...\"))            ; infer name + vc-install"
  :shorthand (lambda (form)
               (let ((arg (cadr form)))
                 (cond
                  ((null arg) nil)
                  ((and (consp arg) (keywordp (car arg))) nil)
                  ((consp arg) (car arg))
                  (t arg)))))

(provide 'init-setup)
;;; init-setup.el ends here
