{
  description = "Demacs Emacs overlay: emacs-git and emacs-igc from source";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Emacs source branches (update independently)
    emacs-src-git = {
      url = "github:emacs-mirror/emacs/master";
      flake = false;
    };
    emacs-src-igc = {
      url = "github:emacs-mirror/emacs/feature/igc";
      flake = false;
    };

    # Package system sub-flake
    demacs-packages = {
      url = "path:../packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      demacs-packages,
      ...
    }@inputs:
    {
      # Composed overlay: packages first, then emacs builds
      overlays.default = nixpkgs.lib.composeManyExtensions [
        demacs-packages.overlays.default
        (import ./emacs.nix { inherit inputs; })
      ];

      # Individual overlays for advanced use
      overlays.emacs-only = import ./emacs.nix { inherit inputs; };
      overlays.packages-only = demacs-packages.overlays.default;
    };
}
