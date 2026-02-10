let
  pkgs = import <nixpkgs> { };
  # Use the package overlay to get updated melpa
  overlay = import ../../overlay.nix { inputs = { }; };
  pkgsWithOverlay = import <nixpkgs> {
    overlays = [ overlay ];
  };
in
{
  inherit (pkgsWithOverlay.emacs.pkgs) melpaStablePackages melpaPackages;
}
