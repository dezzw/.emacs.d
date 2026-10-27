# Work around nixpkgs melpa2nix.el / elpa2nix.el leaving package arguments on
# `command-line-args-left', which makes Emacs visit them after -f returns and
# fail on Darwin batch builds with "Unknown terminal type".
final: prev:
let
  melpa2nix = prev.writeTextFile {
    name = "melpa2nix.el";
    text = builtins.readFile ./melpa2nix.el;
  };
  elpa2nix = prev.writeTextFile {
    name = "elpa2nix.el";
    text = builtins.readFile ./elpa2nix.el;
  };
in
{
  callPackage =
    path: args:
    if builtins.isFunction path then
      prev.callPackage path args
    else
      let
        result = prev.callPackage path args;
        pathStr = toString path;
      in
      if prev.lib.hasInfix "emacs/build-support/melpa.nix" pathStr then
        melpaBuildArgs: result (melpaBuildArgs // {
          melpa2nix = melpaBuildArgs.melpa2nix or melpa2nix;
          elpa2nix = melpaBuildArgs.elpa2nix or elpa2nix;
        })
      else
        result;
}
