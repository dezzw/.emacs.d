# Clear leftover `command-line-args-left' after melpa2nix/elpa2nix and
# emacs-with-packages native batch compile. Without this, Darwin NS Emacs
# revisits those paths in command-line-1 and fails with "Unknown terminal type".
final: prev:
let
  lib = prev.lib;
  buildSupport = "${prev.path}/pkgs/applications/editors/emacs/build-support";

  insertAfter =
    text: needle: insertion:
    let
      parts = lib.splitString needle text;
    in
    if parts == [ text ] then
      throw "emacs-batch-cli-fix-overlay: pattern not found: ${needle}"
    else
      lib.concatStringsSep (needle + insertion) parts;

  melpa2nixEl = prev.writeTextFile {
    name = "melpa2nix-cli-fix.el";
    text = insertAfter
      (builtins.readFile "${buildSupport}/melpa2nix.el")
      "(`(,package ,version ,commit)\n     "
      "(setq command-line-args-left nil)\n     ";
  };

  elpa2nixEl = prev.writeTextFile {
    name = "elpa2nix-cli-fix.el";
    text = insertAfter
      (builtins.readFile "${buildSupport}/elpa2nix.el")
      "(`(,archive ,elpa ,turn-compilation-warning-to-error ,ignore-compilation-error)\n     "
      "(setq command-line-args-left nil)\n     ";
  };

  wrapperSrc = prev.runCommand "emacs-wrapper-batch-cli-fix-src" { } ''
    mkdir -p $out
    cp ${buildSupport}/wrapper.nix ${buildSupport}/wrapper.sh ${buildSupport}/mk-wrapper-subdirs.el $out/
    substituteInPlace $out/wrapper.nix \
      --replace-fail \
        '-f batch-native-compile "$siteStart" "$subdirs"' \
        '-f batch-native-compile "$siteStart" "$subdirs" --eval "(setq command-line-args-left nil)"'
  '';

  emacsPackagesFor' = emacs: pkgSet:
    pkgSet.overrideScope (self: super: {
      melpaBuild =
        args:
        super.melpaBuild (
          args
          // {
            melpa2nix = args.melpa2nix or melpa2nixEl;
            elpa2nix = args.elpa2nix or elpa2nixEl;
          }
        );
      elpaBuild =
        args:
        super.elpaBuild (
          args
          // {
            elpa2nix = args.elpa2nix or elpa2nixEl;
          }
        );
      emacsWithPackages = (prev.callPackage "${wrapperSrc}/wrapper.nix" {
        inherit (prev) lndir;
        inherit lib;
      }) self;
      withPackages = self.emacsWithPackages;
    });
in
{
  emacsPackagesFor = emacs: emacsPackagesFor' emacs (prev.emacsPackagesFor emacs);
}
