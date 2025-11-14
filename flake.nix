{
  description = "Desmond's Emacs (demacs) Configuration";
  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://demacs.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "demacs.cachix.org-1:KwSnWI5wdJm4TGdeUfmksk59098voqdDkBVNrUS7yN4="
    ];
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Emacs patches
    emacs-plus-patches = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };
    emacs-patches = {
      url = "github:LuciusChen/.emacs.d";
      flake = false;
    };

    # Custom Emacs packages from GitHub
    ai-code-interface = {
      url = "github:tninja/ai-code-interface.el";
      flake = false;
    };
    awesome-tray = {
      url = "github:manateelazycat/awesome-tray";
      flake = false;
    };
    claude-code-ide = {
      url = "github:manzaltu/claude-code-ide.el";
      flake = false;
    };
    eglot-x = {
      url = "github:nemethf/eglot-x";
      flake = false;
    };
    emt = {
      url = "github:roife/emt";
      flake = false;
    };
    image-slicing = {
      url = "github:ginqi7/image-slicing";
      flake = false;
    };
    leetcode-emacs = {
      url = "github:ginqi7/leetcode-emacs";
      flake = false;
    };
    monet = {
      url = "github:stevemolitor/monet";
      flake = false;
    };
    org-modern-indent = {
      url = "github:jdtsmith/org-modern-indent";
      flake = false;
    };
    panel = {
      url = "github:LuciusChen/panel";
      flake = false;
    };
    rose-pine = {
      url = "github:LuciusChen/rose-pine";
      flake = false;
    };
    agent-shell-sidebar = {
      url = "github:cmacrae/agent-shell-sidebar";
      flake = false;
    };
    setup-el = {
      url = "git+https://codeberg.org/pkal/setup.el.git";
      flake = false;
    };
    telega = {
      url = "github:LuciusChen/telega.el";
      flake = false;
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      emacs-overlay,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };
        lib = pkgs.lib;
      in
      rec {
        languageServers = with pkgs; [
          ccls

          ruff
          basedpyright

          vscode-langservers-extracted
          typescript-language-server
          bash-language-server

          neil
          clj-kondo

          nixd
          texlab
          lua-language-server
          fennel-ls

          # "${pkgs.vscode-extensions.ms-vscode.cpptools}/share/vscode/extensions/ms-vscode.cpptools/debugAdapters"
        ];

        # Remove MPS from emacs-igc (it's now built into the emacs repo)
        emacs-no-mps = pkgs.emacs-igc.overrideAttrs (old: {
          buildInputs = builtins.filter (p: !(p ? pname && p.pname == "mps")) (old.buildInputs or [ ]);
        });

        # Add macOS-specific patches and ImageMagick
        emacs-patched =
          (emacs-no-mps.override {
            withImageMagick = true;
          }).overrideAttrs
            (old: {
              patches = (old.patches or [ ]) ++ [
                # Add setting to enable rounded window with no decoration (still have to alter default-frame-alist)
                "${inputs.emacs-plus-patches}/patches/emacs-31/round-undecorated-frame.patch"

                # Make Emacs aware of OS-level light/dark mode
                "${inputs.emacs-plus-patches}/patches/emacs-31/system-appearance.patch"

                # Custom patches
                # "${inputs.emacs-patches}/patches/emacs-31/ns-alpha-background.patch"
                # "${inputs.emacs-patches}/patches/emacs-31/smooth-cursor.patch"
              ];
            });

        # Use emacs-no-mps on Linux, patched version on Darwin
        emacs-base = if pkgs.stdenv.isLinux then emacs-no-mps else emacs-patched;

        emacs-augmented = (
          (pkgs.emacsPackagesFor emacs-base).emacsWithPackages (
            epkgs:
            with epkgs;
            let
              # Helper to convert timestamp to date string (YYYYMMDD format)
              timestampToDate =
                timestamp:
                let
                  inherit (builtins) substring;
                  dateStr = builtins.readFile (
                    pkgs.runCommand "timestamp-to-date" { } ''
                      date -u -d @${toString timestamp} +%Y%m%d > $out
                    ''
                  );
                in
                lib.removeSuffix "\n" dateStr;

              # Custom package derivations
              customPackages = {
                agent-shell-sidebar = epkgs.trivialBuild {
                  pname = "agent-shell-sidebar";
                  version = timestampToDate inputs.agent-shell-sidebar.lastModified;
                  src = inputs.agent-shell-sidebar;
                  packageRequires = [ agent-shell ];
                };

                ai-code-interface = epkgs.melpaBuild {
                  pname = "ai-code-interface";
                  version = timestampToDate inputs.ai-code-interface.lastModified;
                  src = inputs.ai-code-interface;
                  packageRequires = [
                    magit
                    claude-code
                  ];
                  recipe = pkgs.writeText "recipe" ''
                    (ai-code-interface :repo "tninja/ai-code-interface.el"
                                       :fetcher github
                                       :files ("*.el" (:exclude "test_*.el")))
                  '';
                };

                awesome-tray = epkgs.trivialBuild {
                  pname = "awesome-tray";
                  version = timestampToDate inputs.awesome-tray.lastModified;
                  src = inputs.awesome-tray;
                };

                claude-code-ide = (
                  epkgs.trivialBuild {
                    pname = "claude-code-ide";
                    version = timestampToDate inputs.claude-code-ide.lastModified;
                    src = inputs.claude-code-ide;
                  }
                );

                eglot-x = epkgs.trivialBuild {
                  pname = "eglot-x";
                  version = timestampToDate inputs.eglot-x.lastModified;
                  src = inputs.eglot-x;
                };

                emt = epkgs.trivialBuild {
                  pname = "emt";
                  version = timestampToDate inputs.emt.lastModified;
                  src = inputs.emt;
                };

                image-slicing = epkgs.trivialBuild {
                  pname = "image-slicing";
                  version = timestampToDate inputs.image-slicing.lastModified;
                  src = inputs.image-slicing;
                  packageRequires = [ f ];
                };

                leetcode-emacs = epkgs.trivialBuild {
                  pname = "leetcode-emacs";
                  version = timestampToDate inputs.leetcode-emacs.lastModified;
                  src = inputs.leetcode-emacs;
                };

                monet = epkgs.trivialBuild {
                  pname = "monet";
                  version = timestampToDate inputs.monet.lastModified;
                  src = inputs.monet;
                  packageRequires = [ websocket ];
                };

                org-modern-indent = epkgs.trivialBuild {
                  pname = "org-modern-indent";
                  version = timestampToDate inputs.org-modern-indent.lastModified;
                  src = inputs.org-modern-indent;
                };

                panel = epkgs.trivialBuild {
                  pname = "panel";
                  version = timestampToDate inputs.panel.lastModified;
                  src = inputs.panel;
                  packageRequires = [
                    async
                    nerd-icons
                  ];
                };

                rose-pine = epkgs.trivialBuild {
                  pname = "rose-pine";
                  version = timestampToDate inputs.rose-pine.lastModified;
                  src = inputs.rose-pine;
                };

                setup = epkgs.trivialBuild {
                  pname = "setup";
                  version = timestampToDate inputs.setup-el.lastModified;
                  src = inputs.setup-el;
                };

                telega =
                  let
                    version = timestampToDate inputs.telega.lastModified;
                  in
                  epkgs.melpaBuild {
                    pname = "telega";
                    inherit version;
                    src = inputs.telega;
                    packageRequires = [ visual-fill-column ];
                    buildInputs = [ pkgs.tdlib ];
                    nativeBuildInputs = [
                      pkgs.gnumake
                      pkgs.gcc
                      pkgs.pkg-config
                    ];
                    postPatch = ''
                      # Set telega-server-libs-prefix to tdlib path
                      substituteInPlace telega-customize.el \
                        --replace-fail '(defcustom telega-server-libs-prefix "/usr/local"' \
                                       '(defcustom telega-server-libs-prefix "${pkgs.tdlib}"'
                      # Set telega-server-command to absolute path in the output
                      substituteInPlace telega-customize.el \
                        --replace-fail '(defcustom telega-server-command "telega-server"' \
                                       "(defcustom telega-server-command \"$out/share/emacs/site-lisp/elpa/telega-${version}/telega-server\""
                    '';
                    preBuild = ''
                      make -C server clean
                      make -C server install \
                        LIBS_PREFIX=${pkgs.tdlib} \
                        INSTALL_PREFIX=$out/share/emacs/site-lisp/elpa/telega-${version}
                    '';
                    recipe = pkgs.writeText "telega-recipe" ''
                      (telega :repo "LuciusChen/telega.el"
                              :fetcher github
                              :files (:defaults "Makefile" "etc" "server" "contrib"))
                    '';
                  };
              };
            in
            [
              # Native compiled packages
              vterm
              pdf-tools
              pkgs.emacsPackages.treesit-grammars.with-all-grammars

              # Custom GitHub packages
              customPackages.agent-shell-sidebar
              customPackages.ai-code-interface
              customPackages.awesome-tray
              customPackages.claude-code-ide
              customPackages.eglot-x
              customPackages.emt
              customPackages.image-slicing
              customPackages.leetcode-emacs
              customPackages.monet
              customPackages.org-modern-indent
              customPackages.panel
              customPackages.rose-pine
              customPackages.setup
              customPackages.telega

              # MELPA packages - Core
              eglot-booster
              claude-code
              eat
              meow
              gptel
              ultra-scroll
              indent-bars
              vertico-posframe
              nov
              sis
              plz
              avy
              mpv
              cape
              wgrep
              nerd-icons
              corfu
              company
              vundo
              forge
              verb
              elfeed
              popper
              embark
              dimmer
              vertico
              diredfl
              separedit
              cdlatex
              consult
              mmm-mode
              scratch
              diff-hl
              goggles
              web-mode
              js2-mode
              move-dup
              diminish
              doom-modeline
              git-link
              apheleia
              ox-pandoc
              macrostep
              json-mode
              orderless
              kind-icon
              git-modes
              git-blamed
              ace-pinyin
              marginalia
              rainbow-mode
              prettier-js
              vterm-toggle
              language-detection
              meow-tree-sitter
              markdown-mode
              mode-line-bell
              embark-consult
              speed-type
              typescript-mode
              nerd-icons-dired
              command-log-mode
              browse-kill-ring
              rainbow-delimiters
              default-text-scale
              denote
              nerd-icons-corfu
              nerd-icons-completion
              whitespace-cleanup-mode
              eshell-syntax-highlighting
              consult-dir
              dirvish
              swift-mode
              color-theme-sanityinc-tomorrow
              highlight-parentheses
              yasnippet

              # MELPA packages - Org-mode
              org-modern
              org-appear
              org-remark
              org-tidy
              org-cliplink
              org-download
              visual-fill-column
              valign
              ob-async
              denote-org
              denote-markdown

              # MELPA packages - Language support
              clojure-ts-mode
              cider
              babashka
              neil
              auctex
              fennel-mode
              nix-ts-mode
              geiser-chez

              # MELPA packages - Utilities
              zoom
              activities
              citre
              jinx
              envrc
              helpful
              aggressive-indent
              browser-hist
              consult-notes
              consult-gh
              sideline-blame
              agent-shell
              all-the-icons
              reformatter
              flymake-ruff
              eldoc-box
              undo-fu
              undo-fu-session
            ]
          )
        );

        packages.demacs = emacs-augmented;

        apps.demacs = flake-utils.lib.mkApp {
          drv = packages.demacs;
          name = "demacs";
          exePath = "/bin/emacs";
        };
        packages.default = packages.demacs;
        apps.default = apps.demacs;
      }
    );
}
