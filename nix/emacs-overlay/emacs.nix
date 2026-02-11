# Demacs Emacs build overlay.
# Provides emacs-git (master) and emacs-igc (feature/igc) built from source.
# Sources come from flake inputs -- no JSON files needed.
{ inputs }:

self: super:
let
  # Pure Nix version helper
  versionOf = input: builtins.substring 0 8 input.lastModifiedDate;

  # ===========================================================================
  # mkGitEmacs: build Emacs from a flake git source input
  # ===========================================================================
  mkGitEmacs =
    namePrefix: srcInput: branch:
    { ... }@args:
    let
      version = "${versionOf srcInput}.0";
      rev = srcInput.rev or "unknown";
    in
    builtins.foldl' (drv: fn: fn drv) super.emacs [

      # Step 1: Override build options
      (
        drv:
        drv.override (
          {
            srcRepo = true;
            withXwidgets = super.stdenv.hostPlatform.isDarwin;
          }
          // (
            if super.stdenv.hostPlatform.isDarwin then
              {
                withNS = true;
              }
            else
              { }
          )
          // args
        )
      )

      # Step 2: Override source, version, patches, and native-comp paths
      (
        drv:
        drv.overrideAttrs (old: {
          name = "${namePrefix}-${version}";
          inherit version;
          src = srcInput;

          patches = [ ];

          # Fix segfaults on aarch64-linux (#264)
          configureFlags =
            old.configureFlags
            ++ super.lib.optionals (super.stdenv.isLinux && super.stdenv.isAarch64) [
              "--enable-check-lisp-object-type"
            ];

          postPatch =
            old.postPatch
            + ''
              substituteInPlace lisp/loadup.el \
                --replace-warn '(emacs-repository-get-version)' '"${rev}"' \
                --replace-warn '(emacs-repository-get-branch)' '"${branch}"'
            ''
            +
              # Native compilation: set libgccjit backend paths
              (super.lib.optionalString ((old ? NATIVE_FULL_AOT) || (old ? env.NATIVE_FULL_AOT)) (
                let
                  backendPath = (
                    super.lib.concatStringsSep " " (
                      builtins.map (x: ''\"-B${x}\"'') (
                        [
                          # Paths necessary so the JIT compiler finds its libraries:
                          "${super.lib.getLib self.libgccjit}/lib"
                          "${super.lib.getLib self.libgccjit}/lib/gcc"
                          "${super.lib.getLib self.stdenv.cc.libc}/lib"
                        ]
                        ++ super.lib.optionals (self.stdenv.cc ? cc.libgcc) [
                          "${super.lib.getLib self.stdenv.cc.cc.libgcc}/lib"
                        ]
                        ++ [
                          # Executable paths necessary for compilation (ld, as):
                          "${super.lib.getBin self.stdenv.cc.cc}/bin"
                          "${super.lib.getBin self.stdenv.cc.bintools}/bin"
                          "${super.lib.getBin self.stdenv.cc.bintools.bintools}/bin"
                        ]
                        ++ super.lib.optionals (self.stdenv.hostPlatform.isDarwin && self ? apple-sdk) [
                          # The linker needs to know where to find libSystem on Darwin.
                          "${self.apple-sdk.sdkroot}/usr/lib"
                        ]
                      )
                    )
                  );
                in
                ''
                  substituteInPlace lisp/emacs-lisp/comp.el --replace-warn \
                      "(defcustom comp-libgccjit-reproducer nil" \
                      "(setq native-comp-driver-options '(${backendPath}))
                  (defcustom comp-libgccjit-reproducer nil"
                ''
              ));
        })
      )

      # Step 3: Reconnect passthru.pkgs to the built emacs
      (
        drv:
        let
          result = drv.overrideAttrs (old: {
            passthru = old.passthru // {
              pkgs = self.emacsPackagesFor result;
            };
          });
        in
        result
      )
    ];

  # ===========================================================================
  # Emacs variants
  # ===========================================================================

  emacs-git =
    let
      base = (mkGitEmacs "emacs-git" inputs.emacs-src-git "master") { };
      emacs = emacs-git;
    in
    base.overrideAttrs (oa: {
      passthru = oa.passthru // {
        pkgs = oa.passthru.pkgs.overrideScope (eself: esuper: { inherit emacs; });
      };
    });

  # IGC: no external MPS needed -- it's built into the feature/igc branch now
  emacs-igc =
    let
      base = (mkGitEmacs "emacs-igc" inputs.emacs-src-igc "feature/igc") { };
      emacs = emacs-igc;
    in
    base.overrideAttrs (oa: {
      passthru = oa.passthru // {
        pkgs = oa.passthru.pkgs.overrideScope (eself: esuper: { inherit emacs; });
      };
    });

in
{
  inherit emacs-git emacs-igc;
}
