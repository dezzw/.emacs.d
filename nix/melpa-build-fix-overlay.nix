# Work around Emacs batch helpers leaving paths on `command-line-args-left' after
# `-f' returns, which makes command-line-1 visit them and fail on Darwin batch
# builds with "Unknown terminal type".
final: prev:
let
  lib = prev.lib;
  emacsWrapperDir = ./emacs-wrapper;

  melpa2nix = prev.writeTextFile {
    name = "melpa2nix.el";
    text = builtins.readFile ./melpa2nix.el;
  };
  elpa2nix = prev.writeTextFile {
    name = "elpa2nix.el";
    text = builtins.readFile ./elpa2nix.el;
  };

  patchedWrapperNix = emacsWrapperDir + "/wrapper.nix";

  pathMatches =
    pathStr: fragment:
    lib.hasInfix fragment pathStr;

  wrapCallPackage =
    path: args:
    if builtins.isFunction path then
      prev.callPackage path args
    else
      let
        result = prev.callPackage path args;
        pathStr = toString path;
        melpaMatched = pathMatches pathStr "emacs/build-support/melpa.nix";
        elpaMatched = pathMatches pathStr "emacs/build-support/elpa.nix";
        wrapperMatched = pathMatches pathStr "emacs/build-support/wrapper.nix";
      in
      if melpaMatched then
        melpaBuildArgs: result (melpaBuildArgs // {
          melpa2nix = melpaBuildArgs.melpa2nix or melpa2nix;
          elpa2nix = melpaBuildArgs.elpa2nix or elpa2nix;
        })
      else if elpaMatched then
        elpaBuildArgs: result (elpaBuildArgs // {
          elpa2nix = elpaBuildArgs.elpa2nix or elpa2nix;
        })
      else if wrapperMatched then
        prev.callPackage patchedWrapperNix args
      else
        result;
in
assert lib.hasInfix "setq command-line-args-left nil" (builtins.readFile patchedWrapperNix);
{
  callPackage = wrapCallPackage;
  demacsEmacsBatchCliFixesApplied = true;
}
