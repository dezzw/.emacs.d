# Demacs package overlay
# Overrides emacsPackagesFor with:
#   1. Updated MELPA/ELPA/Nongnu package sources (from repos/)
#   2. Custom packages injected into the epkgs scope (from custom.nix)
{ inputs }:

self: super:
let
  customPackagesFn = import ./custom.nix { inherit inputs; };
in
{
  emacsPackagesFor =
    emacs:
    (
      (super.emacsPackagesFor emacs).overrideScope (
        eself: esuper:
        let
          # =================================================================
          # Updated MELPA/ELPA/Nongnu package sources
          # =================================================================
          melpaStablePackages = esuper.melpaStablePackages.override {
            archiveJson = ./repos/melpa/recipes-archive-melpa.json;
          };

          melpaPackages = esuper.melpaPackages.override {
            archiveJson = ./repos/melpa/recipes-archive-melpa.json;
          };

          elpaDevelPackages = esuper.elpaDevelPackages.override {
            generated = ./repos/elpa/elpa-devel-generated.nix;
          };

          elpaPackages =
            (esuper.elpaPackages.override {
              generated = ./repos/elpa/elpa-generated.nix;
            })
            // {
              # TODO: Remove when ELPA publishes a Tramp version > 2.8.0.4
              # Tramp 2.8.0.4 has a broken tarball
              tramp =
                if esuper.elpaPackages.tramp.version != "2.8.0.4" then
                  esuper.elpaPackages.tramp
                else
                  esuper.elpaPackages.tramp.overrideAttrs {
                    version = "2.8.0.3";
                    src = self.fetchurl {
                      name = "tramp-2.8.0.3.tar";
                      url = "https://elpa.gnu.org/packages/tramp-2.8.0.3.tar.lz";
                      downloadToTemp = true;
                      postFetch = ''
                        cp $downloadedFile tramp-2.8.0.3.tar.lz
                        ${self.lib.getExe self.lzip} -d tramp-2.8.0.3.tar.lz
                        mv tramp-2.8.0.3.tar $out
                      '';
                      hash = "sha256-o+heQw47btZhhM+5GtvzUZlqcNaoW3966fZyj8m6X+M=";
                    };
                  };
            };

          nongnuDevelPackages = esuper.nongnuDevelPackages.override {
            generated = ./repos/nongnu/nongnu-devel-generated.nix;
          };

          nongnuPackages = esuper.nongnuPackages.override {
            generated = ./repos/nongnu/nongnu-generated.nix;
          };

          # =================================================================
          # Custom packages injected into epkgs scope
          # =================================================================
          customPkgs = customPackagesFn {
            pkgs = self;
            epkgs = eself;
          };

        in
        (esuper.override {
          inherit
            melpaStablePackages
            melpaPackages
            elpaDevelPackages
            elpaPackages
            nongnuDevelPackages
            nongnuPackages
            ;
        })
        // customPkgs
      )
    );
}
