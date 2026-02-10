let
  pkgs = import <nixpkgs> { };
  overlay = import ../../overlay.nix { inputs = { }; };
  pkgsWithOverlay = import <nixpkgs> {
    overlays = [ overlay ];
  };
in
pkgsWithOverlay.emacs.pkgs
