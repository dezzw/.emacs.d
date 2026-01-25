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

    org-modern-indent = {
      url = "github:jdtsmith/org-modern-indent";
      flake = false;
    };
    panel = {
      url = "github:LuciusChen/panel";
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
    tdlib = {
      url = "github:tdlib/td";
      flake = false;
    };
    blame-reveal = {
      url = "github:LuciusChen/blame-reveal";
      flake = false;
    };
    lsp-proxy = {
      url = "github:jadestrong/lsp-proxy";
      flake = false;
    };
    symbol-overlay = {
      url = "github:roife/symbol-overlay";
      flake = false;
    };
    consult-ripfd = {
      url = "github:jdtsmith/consult-ripfd";
      flake = false;
    };
    tree-sitter-clojure = {
      url = "github:sogaiu/tree-sitter-clojure?ref=unstable-20250526";
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
        # ============================================================================
        # Helper Functions
        # ============================================================================

        # Helper to convert timestamp to date string (YYYYMMDD format)
        timestampToDate =
          timestamp:
          let
            dateStr = builtins.readFile (
              pkgs.runCommand "timestamp-to-date" { } ''
                date -u -d @${toString timestamp} +%Y%m%d > $out
              ''
            );
          in
          lib.removeSuffix "\n" dateStr;

        # Function to apply patches to an Emacs derivation
        applyPatches =
          emacs-base: extraPatches:
          emacs-base.overrideAttrs (old: {
            patches = (old.patches or [ ]) ++ extraPatches;
          });

        # Custom patches list (can be enabled/disabled as needed)
        customPatches = [
          # Add setting to enable rounded window with no decoration
          "${inputs.emacs-plus-patches}/patches/emacs-31/round-undecorated-frame.patch"
          # Make Emacs aware of OS-level light/dark mode
          "${inputs.emacs-plus-patches}/patches/emacs-31/system-appearance.patch"
          # Custom patches
          "${inputs.emacs-patches}/patches/emacs-31/ns-alpha-background.patch"
          "${inputs.emacs-patches}/patches/emacs-31/smooth-cursor.patch"
        ];

        # ============================================================================
        # Emacs Base Versions
        # ============================================================================

        # IGC base: Remove MPS (now built into emacs repo)
        emacs-igc-base = pkgs.emacs-igc.overrideAttrs (old: {
          buildInputs = builtins.filter (p: !(p ? pname && p.pname == "mps")) (old.buildInputs or [ ]);
        });

        # Master base: Use emacsGit from emacs-overlay (has vc-run-delayed-success)
        emacs-master-base = pkgs.emacs-git;

        # ============================================================================
        # Emacs Patched Versions
        # ============================================================================

        # IGC patched: Add ImageMagick and custom patches
        emacs-igc-patched = applyPatches (emacs-igc-base.override {
          withImageMagick = true;
        }) customPatches;

        # Master patched: Add ImageMagick and custom patches
        emacs-master-patched = applyPatches (emacs-master-base.override {
          withImageMagick = true;
        }) customPatches;

        # ============================================================================
        # Build Dependencies
        # ============================================================================

        # Build tdlib from HEAD source
        tdlib-head = pkgs.tdlib.overrideAttrs (old: {
          version = "head-${timestampToDate inputs.tdlib.lastModified}";
          src = inputs.tdlib;
          preConfigure = ''
            rm -rf build
          '';
          enableParallelBuilding = true;
          preBuild = (old.preBuild or "") + ''
            export CMAKE_BUILD_PARALLEL_LEVEL=2
          '';
          makeFlags = (old.makeFlags or [ ]) ++ [ "-j2" ];
        });

        # Build lsp-proxy Rust binary from HEAD source
        emacs-lsp-proxy-binary = pkgs.rustPlatform.buildRustPackage {
          pname = "emacs-lsp-proxy";
          version = "unstable-${timestampToDate inputs.lsp-proxy.lastModified}";
          src = inputs.lsp-proxy;
          cargoLock = {
            lockFile = "${inputs.lsp-proxy}/Cargo.lock";
          };
        };

        # ============================================================================
        # Package Definitions
        # ============================================================================

        # Override tree-sitter clojure grammar
        treesit-grammars-with-clojure-override =
          let
            clojure-grammar = pkgs.tree-sitter.buildGrammar {
              language = "clojure";
              version = "unstable-${timestampToDate inputs.tree-sitter-clojure.lastModified}";
              src = inputs.tree-sitter-clojure;
            };
          in
          pkgs.emacsPackages.treesit-grammars.with-grammars (
            grammars:
            let
              grammarList = builtins.attrValues grammars;
              filtered = builtins.filter (g: g.language or "" != "clojure") grammarList;
            in
            filtered ++ [ clojure-grammar ]
          );

        # Custom package derivations (shared across all Emacs versions)
        customPackages = epkgs: {
          agent-shell-sidebar = epkgs.trivialBuild {
            pname = "agent-shell-sidebar";
            version = timestampToDate inputs.agent-shell-sidebar.lastModified;
            src = inputs.agent-shell-sidebar;
            packageRequires = [ epkgs.agent-shell ];
          };

          consult-ripfd = epkgs.trivialBuild {
            pname = "consult-ripfd";
            version = timestampToDate inputs.consult-ripfd.lastModified;
            src = inputs.consult-ripfd;
            packageRequires = [ epkgs.consult ];
          };

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
            packageRequires = [ epkgs.f ];
          };

          leetcode-emacs = epkgs.trivialBuild {
            pname = "leetcode-emacs";
            version = timestampToDate inputs.leetcode-emacs.lastModified;
            src = inputs.leetcode-emacs;
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
              epkgs.async
              epkgs.nerd-icons
            ];
          };

          setup = epkgs.trivialBuild {
            pname = "setup";
            version = timestampToDate inputs.setup-el.lastModified;
            src = inputs.setup-el;
          };

          lsp-proxy = epkgs.melpaBuild {
            pname = "lsp-proxy";
            version = timestampToDate inputs.lsp-proxy.lastModified;
            src = inputs.lsp-proxy;
            # Patch lsp-proxy-core.el to use the Nix-built binary
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

          symbol-overlay = epkgs.trivialBuild {
            pname = "symbol-overlay";
            version = timestampToDate inputs.symbol-overlay.lastModified;
            src = inputs.symbol-overlay;
          };

          blame-reveal = epkgs.trivialBuild {

            pname = "blame-reveal";
            version = timestampToDate inputs.blame-reveal.lastModified;
            src = inputs.blame-reveal;
          };

          telega =
            let
              version = timestampToDate inputs.telega.lastModified;
            in
            epkgs.melpaBuild {
              pname = "telega";
              inherit version;
              src = inputs.telega;
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

        # Package list (shared across all Emacs versions)
        packageList =
          epkgs: customPkgs: with epkgs; [
            # Native compiled packages
            pdf-tools
            treesit-grammars-with-clojure-override
            vterm

            # Custom GitHub packages
            customPkgs.agent-shell-sidebar
            customPkgs.blame-reveal
            customPkgs.eglot-x
            customPkgs.emt
            customPkgs.image-slicing
            customPkgs.leetcode-emacs
            customPkgs.lsp-proxy
            customPkgs.org-modern-indent
            customPkgs.panel
            customPkgs.setup
            customPkgs.symbol-overlay
            customPkgs.consult-ripfd
            customPkgs.telega

            # MELPA packages - Core
            ace-pinyin
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
            eat
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

            # MELPA packages - Org-mode
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

            # MELPA packages - Language support
            auctex
            babashka
            cider
            clojure-ts-mode
            fennel-mode
            geiser-chez
            neil
            nix-ts-mode

            # MELPA packages - Utilities
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

        # ============================================================================
        # Build Function
        # ============================================================================

        # Function to build emacs-augmented with a given emacs-base
        buildEmacsAugmented =
          emacs-base:
          ((pkgs.emacsPackagesFor emacs-base).emacsWithPackages (
            epkgs:
            with epkgs;
            let
              customPkgs = customPackages epkgs;
            in
            packageList epkgs customPkgs
          ));

        # ============================================================================
        # Build All Versions
        # ============================================================================

        # Build all four versions
        emacs-augmented-igc = buildEmacsAugmented emacs-igc-base;
        emacs-augmented-igc-patched = buildEmacsAugmented emacs-igc-patched;
        emacs-augmented-master = buildEmacsAugmented emacs-master-base;
        emacs-augmented-master-patched = buildEmacsAugmented emacs-master-patched;

        # ============================================================================
        # Package Outputs
        # ============================================================================

        packages.demacs-igc = emacs-augmented-igc;
        packages.demacs-igc-patched = emacs-augmented-igc-patched;
        packages.demacs-master = emacs-augmented-master;
        packages.demacs-master-patched = emacs-augmented-master-patched;

        # Default is IGC for backward compatibility
        packages.demacs = emacs-augmented-igc;
        packages.default = packages.demacs;

        # ============================================================================
        # App Outputs
        # ============================================================================

        mkApp =
          name: drv:
          flake-utils.lib.mkApp {
            inherit drv;
            name = "demacs-${name}";
            exePath = "/bin/emacs";
          };

        apps.demacs-igc = mkApp "igc" packages.demacs-igc;
        apps.demacs-igc-patched = mkApp "igc-patched" packages.demacs-igc-patched;
        apps.demacs-master = mkApp "master" packages.demacs-master;
        apps.demacs-master-patched = mkApp "master-patched" packages.demacs-master-patched;
        apps.demacs = apps.demacs-igc;
        apps.default = apps.demacs;
      }
    );
}
