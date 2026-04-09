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

(setup-define :global-bind
  (lambda (&rest bindings)
    `(progn
       ,@(let (forms)
           (while bindings
             (unless (cdr bindings)
               (error ":global-bind expects KEY COMMAND pairs"))
             (let ((key (pop bindings))
                   (command (pop bindings)))
               (push `(keymap-global-set ,key ,command) forms)))
           (nreverse forms))))
  :documentation "Globally bind one or more KEY COMMAND pairs using `keymap-global-set'.")

(setup-define :set
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
               ',name))
   (lambda (name val)
     `(progn
        (custom-load-symbol ',name)
        (funcall (or (get ',name 'custom-set) #'set-default)
                 ',name ,val))))
  :documentation "Set one or more customizable variables using `setopt'."
  :debug '(sexp form)
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

(setup-define :if-graphic
  (lambda (&rest body)
    `(when (or (display-graphic-p) (daemonp))
       ,@body))
  :documentation "Evaluate BODY only when Emacs is graphical, or running as a daemon."
  :indent 0)

(provide 'init-setup)
;;; init-setup.el ends here
