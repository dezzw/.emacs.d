{
  description = "Desmond's Emacs (demacs) Configuration";
  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://dezzw.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "dezzw.cachix.org-1:5YXdWpaFXkULUAJ30oEaGHCZlC2Tt7SZMW8r9kmR83E="
    ];
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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

        emacs-patched =
          (pkgs.emacs-igc.override {
            withImageMagick = true;
          }).overrideAttrs
            (old: rec {

              buildInputs = builtins.filter (p: !(p ? pname && p.pname == "mps")) (old.buildInputs or []);

              patches = (old.patches or [ ]) ++ [
                # Add setting to enable rounded window with no decoration (still have to alter default-frame-alist)
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-31/round-undecorated-frame.patch";
                  sha256 = "sha256-WWLg7xUqSa656JnzyUJTfxqyYB/4MCAiiiZUjMOqjuY=";
                })

                # Make Emacs aware of OS-level light/dark mode
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-31/system-appearance.patch";
                  sha256 = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
                })

                ./patches/emacs-31/ns-alpha-background.patch
                ./patches/emacs-31/smooth-cursor.patch
              ];
            });

        # Use emacs-igc directly on Linux, patched version on Darwin
        emacs-base = if pkgs.stdenv.isLinux then
          pkgs.emacs-igc
        else
          emacs-patched;

        emacs-augmented = (
          (pkgs.emacsPackagesFor emacs-base).emacsWithPackages (
            epkgs: with epkgs; [
              # (callPackage ./site-packages/lsp-bridge/lsp-bridge.nix {
              #   inherit (pkgs) fetchFromGitHub;
              # })

              vterm
              pdf-tools
              pkgs.emacsPackages.treesit-grammars.with-all-grammars
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
