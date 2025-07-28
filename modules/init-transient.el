;;; init-transient.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load lib-transient)
    (:global    "C-c e a" agenda-transient
                "C-c e j" journal-transient
                "C-c e e" emacs-access-transient
                "C-c e g" gptel-menu
                "C-c e p" prog-commands
                "C-c e m" magit-commands
                "C-c e u" uniline-transient
                "C-c e d" dape-transient
                "C-c e i" projectile-transient)
    (:with-map transient-base-map
      (:bind "<escape>" transient-quit-one))
    (:option transient-semantic-coloring t)

    ;; org daily
    (transient-define-prefix journal-transient ()
      "Journal menu"
      :info-manual "Journal menu"
      ["Arguments"
       ("-j" "Journal"            "journal.org")
       ("-d" "Clear archive log"  "delete")]
      ["Commands"
       ("RET" "Journal files switch"   journal-options)]
      [("q" "Quit"           transient-quit-one)])

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
        ("i" "find-impl"             eglot-find-implementation)
        ("s" "find-symbols"          xref-find-apropos)
        ("x" "find-mapper-xml"       +java-to-xml-mapper)
        ("o" "find-def-other-window" xref-find-definitions-other-window)
        ]
       ["Code action"
        ("a" "code-actions"      eglot-code-actions)
        ("r" "rename"            eglot-rename)
        ("f" "format-all-buffer" apheleia-format-buffer)]
       ["diagnostic"
        ("n" "jump-to-next-diagnostic" flymake-goto-next-error)
        ("N" "jump-to-prev-diagnostic" flymake-goto-prev-error)
        ("l" "list-diagnostics"        consult-flymake)]
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
        ("M-n" "next sibling section"     magit-section-forward-sibling)]])

    ;; dape
    (transient-define-prefix dape-transient ()
      "Transient for dape."
      [["Stepping"
        ("n" "Next"     dape-next     :transient t)
        ("i" "Step in"  dape-step-in  :transient t)
        ("o" "Step out" dape-step-out :transient t)
        ("c" "Continue" dape-continue :transient t)
        ("r" "Restart"  dape-restart  :transient t)]
       ["Breakpoints"
        ("bb" "Toggle"     dape-breakpoint-toggle          :transient t)
        ("bd" "Delete"     dape-breakpoint-remove-at-point :transient t)
        ("bD" "Delete all" dape-breakpoint-remove-all      :transient t)
        ("bl" "Log"        dape-breakpoint-log             :transient t)]
       ["Info"
        ("si" "Info"         dape-info         :transient t)
        ("sm" "Memory"       dape-read-memory  :transient t)
        ("ss" "Select Stack" dape-select-stack :transient t)
        ("R" "Repl"          dape-repl         :transient t)]
       ["Quit"
        ("qq" "Quit" dape-quit :transient nil)
        ("qk" "Kill" dape-kill :transient nil)]])

    (transient-define-prefix projectile-transient ()
      "Projectile command map."
      ["Transient menu for projectile commands."
       ["Command"
        ("c p" "package project" projectile-package-project)
        ("c t" "test project" projectile-test-project)
        ("c r" "run project cmd" projectile-run-project)
        ("c i" "install project" projectile-install-project)
        ("c c" "compile project" projectile-compile-project)
        ("!" "run shell cmd in root" projectile-run-shell-command-in-root)
        ("&" "(async)run shell cmd in root" projectile-run-async-shell-command-in-root)
        ("x 4 v" "run vterm in other window" projectile-run-vterm-other-window)
        ("x v" "run vterm" projectile-run-vterm)
        ("x g" "run gdb" projectile-run-gdb)]
       ["Buffer"
        ("B" "show project buffers" projectile-display-buffer)
        ("l" "ibuffer" projectile-ibuffer)
        ("S" "save buffers" projectile-save-project-buffers)
        ("K" "kill buffers" projectile-kill-buffers)
        ("b" "switch project buffer" projectile-switch-to-buffer)]
       ["File"
        ("o f" "find file in other window" projectile-find-file-other-window)
        ("f" "find file" projectile-find-file)
        ("g" "find file dwim" projectile-find-file-dwim)
        ("t f" "find test files in project" projectile-find-test-file)
        ("t t" "toggle implementation and test" projectile-toggle-between-implementation-and-test)]
       ["Edit"
        ("s x" "references" projectile-find-references)
        ("j" "jump to tag" projectile-find-tag)
        ("e r" "replace" projectile-replace)
        ("e R" "regex replace"  projectile-replace-regexp)]
       ["Directory"
        ("d" "find dir " projectile-find-dir)
        ("o d" "find dir in other window" projectile-find-dir-other-window)
        ("D" "dired" projectile-dired)
        ("o D" "dired in other window" projectile-dired-other-window)]
       ["Projectile"
        ("p" "switch project" projectile-switch-project)
        ("q" "switch open project" projectile-switch-open-project)
        ("i" "invalidate cache" projectile-invalidate-cache :transient t)
        ("z" "cache current file" projectile-cache-current-file)]])))

(defvar my-linuxPC-Lock-p "cli455")
(defvar my-linuxPC-Lock-l "0")

(defun my/set-linuxPC-Lock-p ()
  (interactive)
  (setq my-linuxPC-Lock-p (read-string "Port (-p, blank to omit): " my-linuxPC-Lock-p)))

(defun my/set-linuxPC-Lock-l ()
  (interactive)
  (setq my-linuxPC-Lock-l (read-string "Lock (-l, blank to omit): " my-linuxPC-Lock-l)))

(defun my/linuxPC-Lock-build-cmd ()
  (let ((args (list "/home/desmond/workspace/bitstream/test/fpga/linuxPC_Lock.py")))
    (when (and my-linuxPC-Lock-p (not (string-empty-p my-linuxPC-Lock-p)))
      (setq args (append args (list "-p" my-linuxPC-Lock-p))))
    (when (and my-linuxPC-Lock-l (not (string-empty-p my-linuxPC-Lock-l)))
      (setq args (append args (list "-l" my-linuxPC-Lock-l))))
    (format "python3 %s" (mapconcat #'shell-quote-argument args " "))))

(defun my/linuxPC-Lock-run ()
  (interactive)
  (let ((cmd (my/linuxPC-Lock-build-cmd)))
    (when (yes-or-no-p (format "Run command: %s ?" cmd))
      (compile cmd))))

(transient-define-prefix my-linuxPC-Lock ()
  "Edit args and run linuxPC_Lock.py. See final command on run."
  [ ["Arguments"
      ("p" (lambda () (format "-p (Port) [%s]" my-linuxPC-Lock-p)) my/set-linuxPC-Lock-p)
      ("l" (lambda () (format "-l (Lock) [%s]" my-linuxPC-Lock-l)) my/set-linuxPC-Lock-l)]
    ["Actions"
      ("r" "Run" my/linuxPC-Lock-run)] ])

(global-set-key (kbd "C-c L") #'my-linuxPC-Lock)

(provide 'init-transient)
;;; init-transient.el ends here
