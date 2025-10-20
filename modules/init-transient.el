;;; init-transient.el --- Transient configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load lib-transient)
    (:global "C-c e e" emacs-access-transient
             "C-c e g" gptel-menu
             "C-c e p" prog-commands
             "C-c e m" magit-commands
             "C-c e u" uniline-transient
             "C-c e d" dape-transient
             "C-c e l" bstt/lock
             "C-c e w" bstt/webapp-compile
             "C-c e t" bstt/toplevel)
    (:with-map transient-base-map
      (:bind "<escape>" transient-quit-one))
    (:option transient-semantic-coloring t)

    ;; BSTT Lock Command Transient
    (transient-define-prefix bstt/lock ()
      "Edit args and run linuxPC_Lock.py in project_root/webapp directory."
      :info-manual "(bstt-lock) BSTT Lock Command"
      [ ["Arguments (press to modify, menu stays open)"
          ("p" (lambda () (format "Port: %s" bstt/lock-port)) bstt/lock-set-port :transient t)
          ("l" (lambda () (format "Lock: %s" bstt/lock-value)) bstt/lock-set-value :transient t)]
        ["Actions"
          ("RET" "Run command" bstt/lock-run)
          ("q" "Quit menu" transient-quit-one)] ])

    ;; BSTT Webapp Compile Command Transient
    (transient-define-prefix bstt/webapp-compile ()
      "Edit args and run webapp compile command in project_root/webapp directory."
      :info-manual "(bstt-webapp-compile) BSTT Webapp Compile Command"
      [ ["Arguments (press to modify, menu stays open)"
          ("c" (lambda () (format "CLI Code: %s" bstt/webapp-cli-code)) bstt/webapp-compile-set-cli-code :transient t)
          ("b" (lambda () (format "Batch Code: %s" bstt/webapp-batch-code)) bstt/webapp-compile-set-batch-code :transient t)
          ("w" (lambda () (format "Browser: %s" bstt/webapp-browser)) bstt/webapp-compile-set-browser :transient t)
          ("r" (lambda () (format "Repeat: %s" bstt/webapp-repeat)) bstt/webapp-compile-set-repeat :transient t)]
        ["Actions"
          ("RET" "Run command" bstt/webapp-compile-run)
          ("q" "Quit menu" transient-quit-one)] ])

    ;; BSTT Toplevel Command Transient
    (transient-define-prefix bstt/toplevel ()
      "Edit args and run toplevel.py in project_root/bitstreams directory."
      :info-manual "(bstt-toplevel) BSTT Toplevel Command"
      [ ["Arguments (press to modify, menu stays open)"
          ("p" (lambda () (format "CLI Code (-p): %s" bstt/toplevel-cli-code)) bstt/toplevel-set-cli-code :transient t)
          ("l" (lambda () (format "Local Code (--local): %s" bstt/toplevel-local-code)) bstt/toplevel-set-local-code :transient t)
          ("c" (lambda () (format "Config (-c): %s" (if (string-empty-p bstt/toplevel-config) "<none>" bstt/toplevel-config))) bstt/toplevel-set-config :transient t)]
        ["Actions"
          ("RET" "Run command" bstt/toplevel-run)
          ("q" "Quit menu" transient-quit-one)] ])

    ;; file access
    (transient-define-prefix  emacs-access-transient ()
      "Emacs quick access"
      :info-manual "Emacs quick access"
      [["Emacs"
        ("-r" "repos"    "~/.emacs.d/straight/repos/")
        ("-c" "settings" "~/.emacs.d/lisp/")
        ("-p" "projects" "~/IdeaProjects/")]
       ["Files"
        ("-t" "telega" "~/.telega/")
        ("-a" "agenda" "agenda")
        ("-b" "books"  "books")]]
      ["Commands"
       ("RET" "Emacs quick access" browse-path)]
      [("q" "Quit" transient-quit-one)])

    ;; transient 适合大量单一相关的功能需要在 buffer 进行交互的，单纯频次较高的功能按键其实并不适合。
    ;; TODO 需要改为单纯的快捷键
    (transient-define-prefix prog-commands ()
      "Prog commands"
      :info-manual "Prog commands"
      [["Code find"
        ("d" "find-definitions"      xref-find-definitions)
        ("D" "find-references"       xref-find-references)
        ("i" "find-impl"             lsp-find-implementation)
        ("s" "find-symbols"          xref-find-apropos)
        ("x" "find-mapper-xml"       +java-to-xml-mapper)
        ("o" "find-def-other-window" xref-find-definitions-other-window)
        ]
       ["Code action"
        ("a" "code-actions"      lsp-execute-code-action)
        ("r" "rename"            lsp-rename)
        ("f" "format-buffer"     lsp-format-buffer)
        ("R" "format-region"     lsp-format-region)]
       ["diagnostic"
        ("n" "jump-to-next-diagnostic" flycheck-next-error)
        ("N" "jump-to-prev-diagnostic" flycheck-previous-error)
        ("l" "list-diagnostics"        consult-flycheck)]
       ["Navigate"
        ("m" "consult-mark" consult-mark)]])

    (transient-define-prefix magit-commands ()
      "Magit commands"
      :info-manual "Magit commands"
      [["Magit navigate"
        ("n"   "Untracked section"        magit-jump-to-untracked)
        ("u"   "Unstaged section"         magit-jump-to-unstaged)
        ("s"   "Staged section"           magit-jump-to-staged)
        ("p"   "Unpushed section"         magit-jump-to-unpushed-to-pushremote)
        ("M-p" "previous sibling section" magit-section-backward-sibling)
        ("M-n" "next sibling section"     magit-section-forward-sibling)]])))

(provide 'init-transient)
;;; init-transient.el ends here
