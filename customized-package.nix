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
    embr = {
      input = "embr";
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
        // (
          if spec ? packageRequires then
            { packageRequires = epkgsFromNames epkgs spec.packageRequires; }
          else
            { }
        )
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
    epkgs: with epkgs; [
      pdf-tools
      treesit-grammars-with-clojure-override
      vterm
    ];

  melpaUiPackages =
    epkgs: with epkgs; [
      ace-window
      default-text-scale
      diredfl
      dirvish
      doom-modeline
      highlight-parentheses
      mode-line-bell
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      popper
      ultra-scroll
      vertico
      vertico-posframe
      zoom
    ];

  melpaCompletionPackages =
    epkgs: with epkgs; [
      cape
      company
      consult
      consult-dir
      corfu
      embark
      embark-consult
      marginalia
      orderless
      yasnippet
    ];

  melpaEditingPackages =
    epkgs: with epkgs; [
      ace-pinyin
      apheleia
      avy
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

  melpaOrgPackages =
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

  melpaLangPackages =
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

  melpaProgPackages =
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

  melpaVcPackages =
    epkgs: with epkgs; [
      diff-hl
      git-link
      git-modes
      magit
    ];

  melpaAiPackages =
    epkgs: with epkgs; [
      agent-shell
      gptel
    ];

  melpaUtilityPackages =
    epkgs: with epkgs; [
      activities
      eshell-syntax-highlighting
      helpful
      jenkins
      keyfreq
      language-detection
      plz
    ];
in
{
  mkPackageList =
    epkgs:
    (nativePackages epkgs)
    ++ (builtins.attrValues (mkCustomPackages epkgs))
    ++ (melpaUiPackages epkgs)
    ++ (melpaCompletionPackages epkgs)
    ++ (melpaEditingPackages epkgs)
    ++ (melpaOrgPackages epkgs)
    ++ (melpaLangPackages epkgs)
    ++ (melpaProgPackages epkgs)
    ++ (melpaVcPackages epkgs)
    ++ (melpaAiPackages epkgs)
    ++ (melpaUtilityPackages epkgs);
}
