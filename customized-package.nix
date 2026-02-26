{
  pkgs,
  inputs,
  treesit-grammars-with-clojure-override,
  tdlib-head,
  emacs-lsp-proxy-binary,
}:
let
  versionOf = inputName: toString inputs.${inputName}.lastModified;
  srcOf = inputName: inputs.${inputName};

  epkgsFromNames = epkgs: names: builtins.map (name: epkgs.${name}) names;

  # Most custom packages are trivialBuild packages.
  # Add/remove simple packages here.
  simpleCustomSpecs = {
    agent-shell-sidebar = {
      input = "agent-shell-sidebar";
      packageRequires = [ "agent-shell" ];
    };
    blame-reveal = {
      input = "blame-reveal";
    };
    consult-ripfd = {
      input = "consult-ripfd";
      packageRequires = [ "consult" ];
    };
    eat = {
      input = "eat";
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
    leetcode-emacs = {
      input = "leetcode-emacs";
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
      epkgs.trivialBuild (
        {
          inherit pname;
          version = versionOf spec.input;
          src = srcOf spec.input;
        }
        // (if spec ? packageRequires then { packageRequires = epkgsFromNames epkgs spec.packageRequires; } else { })
      )
    ) simpleCustomSpecs;

  mkSpecialCustomPackages = epkgs: {
    lsp-proxy =
      let
        version = versionOf "lsp-proxy";
      in
      epkgs.melpaBuild {
        pname = "lsp-proxy";
        inherit version;
        src = srcOf "lsp-proxy";
        postPatch = ''
          substituteInPlace lsp-proxy-core.el \
            --replace-fail \
            "(executable-find \"emacs-lsp-proxy\")" \
            "\"${emacs-lsp-proxy-binary}/bin/emacs-lsp-proxy\""
        '';
        packageRequires = [
          epkgs.s
          epkgs.eldoc
          epkgs.ht
          epkgs.dash
          epkgs.f
          epkgs.yasnippet
        ];
        recipe = pkgs.writeText "lsp-proxy-recipe" ''
          (lsp-proxy :repo "jadestrong/lsp-proxy"
                     :fetcher github
                     :files ("*.el"))
        '';
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

  nativePackages =
    epkgs:
    with epkgs;
    [
      pdf-tools
      treesit-grammars-with-clojure-override
      vterm
    ];

  melpaCorePackages =
    epkgs:
    with epkgs;
    [
      ace-pinyin
      ace-window
      apheleia
      avy
      browse-kill-ring
      cape
      cdlatex
      company
      consult
      consult-dir
      corfu
      default-text-scale
      denote
      diredfl
      dirvish
      diff-hl
      eglot-booster
      elfeed
      embark
      embark-consult
      eshell-syntax-highlighting
      forge
      git-link
      git-modes
      goggles
      gptel
      gptel-agent
      highlight-parentheses
      indent-bars
      language-detection
      macrostep
      marginalia
      markdown-mode
      meow
      meow-tree-sitter
      mmm-mode
      mode-line-bell
      move-dup
      mpv
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      nov
      orderless
      ox-pandoc
      plz
      popper
      rainbow-delimiters
      rainbow-mode
      separedit
      sis
      speed-type
      swift-mode
      ultra-scroll
      verb
      vertico
      vertico-posframe
      vundo
      web-mode
      whitespace-cleanup-mode
      yasnippet
    ];

  melpaOrgPackages =
    epkgs:
    with epkgs;
    [
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

  melpaLangPackages =
    epkgs:
    with epkgs;
    [
      auctex
      babashka
      cider
      clojure-ts-mode
      fennel-mode
      geiser-chez
      neil
      nix-ts-mode
    ];

  melpaUtilityPackages =
    epkgs:
    with epkgs;
    [
      activities
      agent-shell
      ai-code
      eca
      citre
      consult-notes
      eldoc-box
      envrc
      helpful
      jinx
      jira
      keyfreq
      reformatter
      undo-fu
      undo-fu-session
      zoom
    ];
in
{
  mkPackageList =
    epkgs:
    (nativePackages epkgs)
    ++ (builtins.attrValues (mkCustomPackages epkgs))
    ++ (melpaCorePackages epkgs)
    ++ (melpaOrgPackages epkgs)
    ++ (melpaLangPackages epkgs)
    ++ (melpaUtilityPackages epkgs);
}
