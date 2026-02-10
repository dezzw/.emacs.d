{
  description = "Demacs Emacs overlay: emacs-git and emacs-igc from source";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";

    # Emacs source branches (update independently)
    emacs-src-master = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?ref=master";
      flake = false;
    };
    emacs-src-igc = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?ref=feature/igc";
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
