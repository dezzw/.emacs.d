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

    # Custom Emacs packages from GitHub
    eat = {
      url = "git+https://codeberg.org/Stebalien/emacs-eat.git";
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
    miniline = {
      url = "github:dezzw/miniline.el";
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
          # Local patches for macOS NS build tweaks
          ./patches/ns-alpha-background.patch
          ./patches/ns-mac-input-source.patch
        ];

        # ============================================================================
        # Emacs Base Versions
        # ============================================================================

        # IGC base: Use PGTK on Linux, otherwise regular IGC. Remove MPS (now built into emacs repo)
        emacs-igc-base =
          (if pkgs.stdenv.isLinux then pkgs.emacs-igc-pgtk else pkgs.emacs-igc).overrideAttrs
            (old: {
              buildInputs = builtins.filter (p: !(p ? pname && p.pname == "mps")) (old.buildInputs or [ ]);
            });

        # GIT base: Use PGTK on Linux, otherwise regular emacs-git from emacs-overlay
        emacs-git-base = if pkgs.stdenv.isLinux then pkgs.emacs-git-pgtk else pkgs.emacs-git;

        # ============================================================================
        # Emacs Patched Versions
        # ============================================================================

        # IGC patched: Add ImageMagick where supported plus custom patches
        emacs-igc-patched = applyPatches (emacs-igc-base.override {
          withImageMagick = !pkgs.stdenv.isLinux;
        }) customPatches;

        # GIT patched: Add ImageMagick where supported plus custom patches
        emacs-git-patched = applyPatches (emacs-git-base.override {
          withImageMagick = !pkgs.stdenv.isLinux;
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

        # Centralized package definitions/build recipes/group lists
        packageModule = import ./customized-package.nix {
          inherit
            emacs-lsp-proxy-binary
            inputs
            pkgs
            tdlib-head
            treesit-grammars-with-clojure-override
            ;
        };

        # ============================================================================
        # Build Function
        # ============================================================================

        # Function to build emacs-augmented with a given emacs-base
        buildEmacsAugmented =
          emacs-base:
          ((pkgs.emacsPackagesFor emacs-base).emacsWithPackages (
            epkgs: packageModule.mkPackageList epkgs
          ));

        # ============================================================================
        # Build All Versions
        # ============================================================================

        # Build all four versions
        emacs-augmented-igc = buildEmacsAugmented emacs-igc-base;
        emacs-augmented-igc-patched = buildEmacsAugmented emacs-igc-patched;
        emacs-augmented-git = buildEmacsAugmented emacs-git-base;
        emacs-augmented-git-patched = buildEmacsAugmented emacs-git-patched;

        # ============================================================================
        # Package Outputs
        # ============================================================================

        packages.demacs-igc = emacs-augmented-igc;
        packages.demacs-igc-patched = emacs-augmented-igc-patched;
        packages.demacs-git = emacs-augmented-git;
        packages.demacs-git-patched = emacs-augmented-git-patched;

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
        apps.demacs-git = mkApp "git" packages.demacs-git;
        apps.demacs-git-patched = mkApp "git-patched" packages.demacs-git-patched;
        apps.demacs = apps.demacs-igc;
        apps.default = apps.demacs;
      }
    );
}
