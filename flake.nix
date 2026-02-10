{
  description = "Desmond's Emacs (demacs) Configuration";

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://demacs.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "demacs.cachix.org-1:KwSnWI5wdJm4TGdeUfmksk59098voqdDkBVNrUS7yN4="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";

    # Emacs patches (macOS-specific)
    emacs-plus-patches = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };

    # Emacs overlay sub-flake (includes packages sub-flake)
    demacs-emacs-overlay = {
      url = "path:./nix/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.demacs-emacs-overlay.overlays.default
          ];
        };
        lib = pkgs.lib;
      in
      let
        # ==================================================================
        # Auto-detect packages from setup.el forms
        # ==================================================================

        parseSetup = import ./nix/packages/parse-setup.nix { inherit lib; };

        # Packages detected from (setup NAME ...) and (:pkg ...) forms
        detectedPackages = parseSetup.parsePackagesFromSetupFiles {
          dirs = [ ./modules ];
        };

        # ==================================================================
        # Helper Functions
        # ==================================================================

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

        # ==================================================================
        # Emacs Base Versions
        # ==================================================================

        emacs-igc-base = pkgs.emacs-igc;
        emacs-master-base = pkgs.emacs-git;

        # ==================================================================
        # Emacs Patched Versions
        # ==================================================================

        emacs-igc-patched = applyPatches (
          emacs-igc-base.override { withImageMagick = true; }
        ) customPatches;

        emacs-master-patched = applyPatches (
          emacs-master-base.override { withImageMagick = true; }
        ) customPatches;

        # ==================================================================
        # Package List
        # ==================================================================

        # Packages NOT in any (setup ...) form: transitive deps, sub-features
        # loaded via :also-load, or packages with no dedicated setup form.
        # This list shrinks as you add (setup NAME ...) forms.
        extraPackages =
          epkgs:
          with epkgs;
          [
            # Build artifacts / grammars
            treesit-grammars-with-clojure-override

            # setup.el itself (bootstraps the macro system)
            setup

            # Custom packages loaded via :also-load (no own setup form)
            agent-shell-sidebar
            consult-ripfd
            image-slicing
            org-modern-indent

            # MELPA deps loaded via :also-load / require / transitive
            ace-pinyin
            cdlatex
            company
            diredfl
            eat
            embark-consult
            git-link
            git-modes
            language-detection
            macrostep
            mode-line-bell
            mpv
            nerd-icons-corfu
            nerd-icons-dired
            ox-pandoc
            plz
            swift-mode

            # Org packages (init-org.el has Unicode chars that break fromElisp)
            denote
            org-modern
            denote-markdown
            denote-org
            ob-async
            org-appear
            org-cliplink
            org-download
            org-tidy
            valign
            visual-fill-column

            # Language support (no dedicated setup form)
            auctex
            babashka
            cider
            clojure-ts-mode
            fennel-mode
            geiser-chez
            neil
            nix-ts-mode

            # Utilities without setup forms
            consult-notes
            keyfreq
            undo-fu
            undo-fu-session
          ];

        # Combine auto-detected + extras
        packageList =
          epkgs:
          let
            autoNames = map (p: p.name) detectedPackages;
            autoPkgs = map (name: epkgs.${name}) autoNames;
          in
          autoPkgs ++ (extraPackages epkgs);

        # ==================================================================
        # Build Function
        # ==================================================================

        buildEmacsAugmented =
          emacs-base:
          (pkgs.emacsPackagesFor emacs-base).emacsWithPackages (
            epkgs: packageList epkgs
          );

        mkApp =
          name: drv:
          flake-utils.lib.mkApp {
            inherit drv;
            name = "demacs-${name}";
            exePath = "/bin/emacs";
          };

        # ==================================================================
        # Build All Versions
        # ==================================================================

        emacs-augmented-igc = buildEmacsAugmented emacs-igc-base;
        emacs-augmented-igc-patched = buildEmacsAugmented emacs-igc-patched;
        emacs-augmented-master = buildEmacsAugmented emacs-master-base;
        emacs-augmented-master-patched = buildEmacsAugmented emacs-master-patched;
      in
      rec {
        # ==================================================================
        # Package Outputs
        # ==================================================================

        packages.demacs-igc = emacs-augmented-igc;
        packages.demacs-igc-patched = emacs-augmented-igc-patched;
        packages.demacs-git = emacs-augmented-master;
        packages.demacs-git-patched = emacs-augmented-master-patched;
        packages.demacs = emacs-augmented-igc;
        packages.default = packages.demacs;

        # ==================================================================
        # App Outputs
        # ==================================================================

        apps.demacs-igc = mkApp "igc" packages.demacs-igc;
        apps.demacs-igc-patched = mkApp "igc-patched" packages.demacs-igc-patched;
        apps.demacs-git = mkApp "git" packages.demacs-git;
        apps.demacs = apps.demacs-git;
        apps.default = apps.demacs;
      }
    );
}
