{
  inputs,
  lib,
  pkgs,
}:
let
  # UI enhancements on top of built-in frame/window/minibuffer behavior.
  uiPackages =
    epkgs: with epkgs; [
      ace-window
      default-text-scale
      diredfl
      dirvish
      doom-modeline
      highlight-parentheses
      kitty-graphics
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      panel
      posframe
      popper
      symbol-overlay
      ultra-scroll
      vertico
      vertico-posframe
    ];

  # The retained third-party completion stack.
  completionPackages =
    epkgs: with epkgs; [
      cape
      consult
      consult-dir
      consult-ripfd
      corfu
      embark
      embark-consult
      marginalia
      orderless
      yasnippet
    ];

  # Core editing workflow packages.
  editingPackages =
    epkgs: with epkgs; [
      apheleia
      browse-kill-ring
      goggles
      jinx
      macrostep
      meow
      meow-tree-sitter
      move-dup
      rainbow-delimiters
      rainbow-mode
      separedit
      speed-type
      undo-fu
      undo-fu-session
      vundo
      whitespace-cleanup-mode
    ];

  # Org, notes, and reading/presentation helpers.
  orgPackages =
    epkgs: with epkgs; [
      cdlatex
      consult-notes
      denote
      denote-markdown
      denote-org
      image-slicing
      ob-async
      org-appear
      org-cliplink
      org-download
      org-modern
      org-modern-indent
      org-remark
      org-tidy
      valign
      visual-fill-column
    ];

  # Language major modes and language-specific support.
  languagePackages =
    epkgs: with epkgs; [
      auctex
      babashka
      cider
      clojure-ts-mode
      groovy-mode
      nix-ts-mode
      swift-mode
      web-mode
    ];

  # Programming extras layered on top of built-in project/xref/eglot/flymake.
  programmingPackages =
    epkgs: with epkgs; [
      citre
      eglot-booster
      eglot-x
      eldoc-box
      envrc
      ghostel
      indent-bars
      mmm-mode
      treesit-grammars.with-all-grammars
      topsy
      verb
    ];

  # Version control workflow.
  vcPackages =
    epkgs: with epkgs; [
      blame-reveal
      diff-hl
      git-link
      git-modes
      magit
    ];

  # AI helpers.
  aiPackages =
    epkgs: with epkgs; [
      ai-code
      agent-shell
      gptel
    ];

  # Misc retained utilities and integrations.
  utilityPackages =
    epkgs: with epkgs; [
      eshell-syntax-highlighting
      helpful
      jenkins
      keyfreq
      leetcode-emacs
      telega
      pdf-tools
      tabspaces
    ];
in
{
  mkPackageList =
    epkgs:
    with epkgs;
    [ setup ]
    ++ (editingPackages epkgs)
    ++ (completionPackages epkgs)
    ++ (vcPackages epkgs)
    ++ (orgPackages epkgs)
    ++ (languagePackages epkgs)
    ++ (programmingPackages epkgs)
    ++ (uiPackages epkgs)
    ++ (aiPackages epkgs)
    ++ (utilityPackages epkgs);
}
