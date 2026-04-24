{
  pkgs,
  inputs,
  treesit-grammars-with-clojure-override,
  tdlib-head,
}:
let
  versionOf = inputName: toString inputs.${inputName}.lastModified;
  srcOf = inputName: inputs.${inputName};

  epkgsFromNames = epkgs: names: builtins.map (name: epkgs.${name}) names;

  # Simple custom-source packages that are not coming from the default
  # Emacs package set.
  simpleCustomSpecs = {
    blame-reveal = {
      input = "blame-reveal";
    };
    consult-ripfd = {
      input = "consult-ripfd";
      packageRequires = [ "consult" ];
    };
    kitty-graphics = {
      input = "kitty-graphics";
    };
    eglot-x = {
      input = "eglot-x";
    };
    emt = {
      input = "emt";
    };
    image-slicing = {
      input = "image-slicing";
      packageRequires = [ "f" ];
    };
    miniline = {
      input = "miniline";
      packageRequires = [ "nerd-icons" ];
    };
    org-modern-indent = {
      input = "org-modern-indent";
    };
    panel = {
      input = "panel";
      packageRequires = [
        "async"
        "nerd-icons"
        "plz"
      ];
    };
    setup = {
      input = "setup-el";
    };
    symbol-overlay = {
      input = "symbol-overlay";
    };
  };

  mkSimpleCustomPackages =
    epkgs:
    builtins.mapAttrs (
      pname: spec:
      epkgs.melpaBuild (
        {
          inherit pname;
          version = versionOf spec.input;
          src = srcOf spec.input;
        }
        // (
          if spec ? packageRequires then
            { packageRequires = epkgsFromNames epkgs spec.packageRequires; }
          else
            { }
        )
      )
    ) simpleCustomSpecs;

  mkSpecialCustomPackages = epkgs: {
    leetcode-emacs =
      let
        version = versionOf "leetcode-emacs";
      in
      epkgs.melpaBuild {
        pname = "leetcode-emacs";
        ename = "leetcode";
        inherit version;
        src = srcOf "leetcode-emacs";
      };

    telega =
      let
        version = versionOf "telega";
      in
      epkgs.melpaBuild {
        pname = "telega";
        inherit version;
        src = srcOf "telega";
        ignoreCompilationError = true;
        packageRequires = [ epkgs.visual-fill-column ];
        buildInputs = [
          tdlib-head
          pkgs.zlib
        ];
        nativeBuildInputs = [
          pkgs.gnumake
          pkgs.gcc
          pkgs.pkg-config
        ];
        postPatch = ''
          substituteInPlace telega-customize.el \
            --replace-fail '(defcustom telega-server-libs-prefix "/usr/local"' \
                           '(defcustom telega-server-libs-prefix "${tdlib-head}"'
          substituteInPlace telega-customize.el \
            --replace-fail '(defcustom telega-server-command "telega-server"' \
                           "(defcustom telega-server-command \"$out/share/emacs/site-lisp/elpa/telega-${version}/telega-server\""
        '';
        preBuild = ''
          make -C server clean
          make -C server install \
            LIBS_PREFIX=${tdlib-head} \
            INSTALL_PREFIX=$out/share/emacs/site-lisp/elpa/telega-${version}
        '';
        recipe = pkgs.writeText "telega-recipe" ''
          (telega :repo "LuciusChen/telega.el"
                  :fetcher github
                  :files (:defaults "Makefile" "etc" "server" "contrib"))
        '';
      };
  };

  mkCustomPackages = epkgs: (mkSimpleCustomPackages epkgs) // (mkSpecialCustomPackages epkgs);

  # Native dependencies or packages with non-trivial runtime/build integration.
  nativePackages =
    epkgs: with epkgs; [
      pdf-tools
      treesit-grammars-with-clojure-override
      vterm
      ghostel
    ];

  # UI enhancements on top of built-in frame/window/minibuffer behavior.
  uiPackages =
    epkgs: with epkgs; [
      ace-window
      default-text-scale
      diredfl
      dirvish
      doom-modeline
      highlight-parentheses
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      posframe
      popper
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
      ob-async
      org-appear
      org-cliplink
      org-download
      org-modern
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
      geiser-chez
      markdown-mode
      neil
      nix-ts-mode
      swift-mode
      web-mode
    ];

  # Programming extras layered on top of built-in project/xref/eglot/flymake.
  programmingPackages =
    epkgs: with epkgs; [
      citre
      eglot-booster
      eldoc-box
      envrc
      indent-bars
      mmm-mode
      reformatter
      topsy
      verb
    ];

  # Version control workflow.
  vcPackages =
    epkgs: with epkgs; [
      diff-hl
      git-link
      git-modes
      magit
    ];

  # Optional AI helpers.
  aiPackages =
    epkgs: with epkgs; [
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
      language-detection
      tabspaces
    ];
in
{
  mkPackageList =
    epkgs:
    (nativePackages epkgs)
    ++ (builtins.attrValues (mkCustomPackages epkgs))
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
