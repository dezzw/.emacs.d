# Custom package build recipes for demacs.
# This file defines all custom elisp packages (trivialBuild/melpaBuild)
# and their non-elisp dependencies (tdlib, lsp-proxy binary, tree-sitter).
#
# Called by overlay.nix with { pkgs, epkgs } after the MELPA/ELPA/Nongnu
# override, so epkgs already contains standard package sets.
{ inputs }:

{ pkgs, epkgs }:
let
  # Pure Nix version helper -- no IFD needed.
  # Every flake input has lastModifiedDate as "YYYYMMDDHHMMSS".
  versionOf = input: builtins.substring 0 8 input.lastModifiedDate;

  # ===========================================================================
  # Non-elisp build dependencies
  # ===========================================================================

  # Build tdlib from HEAD source (for telega)
  tdlib-head = pkgs.tdlib.overrideAttrs (old: {
    version = "head-${versionOf inputs.tdlib}";
    src = inputs.tdlib;
    preConfigure = ''
      rm -rf build
    '';
    enableParallelBuilding = true;
    preBuild =
      (old.preBuild or "")
      + ''
        export CMAKE_BUILD_PARALLEL_LEVEL=2
      '';
    makeFlags = (old.makeFlags or [ ]) ++ [ "-j2" ];
  });

  # Build lsp-proxy Rust binary from HEAD source
  emacs-lsp-proxy-binary = pkgs.rustPlatform.buildRustPackage {
    pname = "emacs-lsp-proxy";
    version = "unstable-${versionOf inputs.lsp-proxy}";
    src = inputs.lsp-proxy;
    cargoLock = {
      lockFile = "${inputs.lsp-proxy}/Cargo.lock";
    };
  };

  # Override tree-sitter clojure grammar
  treesit-grammars-with-clojure-override =
    let
      clojure-grammar = pkgs.tree-sitter.buildGrammar {
        language = "clojure";
        version = "unstable-${versionOf inputs.tree-sitter-clojure}";
        src = inputs.tree-sitter-clojure;
      };
    in
    pkgs.emacsPackages.treesit-grammars.with-grammars (
      grammars:
      let
        grammarList = builtins.attrValues grammars;
        filtered = builtins.filter (g: g.language or "" != "clojure") grammarList;
      in
      filtered ++ [ clojure-grammar ]
    );

in
{
  # ===========================================================================
  # Tree-sitter grammars (exposed as an epkgs attribute)
  # ===========================================================================
  treesit-grammars-with-clojure-override = treesit-grammars-with-clojure-override;

  # ===========================================================================
  # Custom elisp packages
  # ===========================================================================
  agent-shell = epkgs.melpaBuild {
    pname = "agent-shell";
    version = versionOf inputs.agent-shell;
    src = inputs.agent-shell;
    packageRequires = [
      epkgs.shell-maker
      epkgs.acp
    ];
    recipe = pkgs.writeText "agent-shell-recipe" ''
      (agent-shell :repo "xenodium/agent-shell"
                   :fetcher github
                   :files ("*.el"))
    '';
  };

  agent-shell-sidebar = epkgs.trivialBuild {
    pname = "agent-shell-sidebar";
    version = versionOf inputs.agent-shell-sidebar;
    src = inputs.agent-shell-sidebar;
    packageRequires = [ epkgs.agent-shell ];
  };

  consult-ripfd = epkgs.trivialBuild {
    pname = "consult-ripfd";
    version = versionOf inputs.consult-ripfd;
    src = inputs.consult-ripfd;
    packageRequires = [ epkgs.consult ];
  };

  eglot-x = epkgs.trivialBuild {
    pname = "eglot-x";
    version = versionOf inputs.eglot-x;
    src = inputs.eglot-x;
  };

  emt = epkgs.trivialBuild {
    pname = "emt";
    version = versionOf inputs.emt;
    src = inputs.emt;
  };

  image-slicing = epkgs.trivialBuild {
    pname = "image-slicing";
    version = versionOf inputs.image-slicing;
    src = inputs.image-slicing;
    packageRequires = [ epkgs.f ];
  };

  leetcode-emacs = epkgs.trivialBuild {
    pname = "leetcode-emacs";
    version = versionOf inputs.leetcode-emacs;
    src = inputs.leetcode-emacs;
  };

  org-modern-indent = epkgs.trivialBuild {
    pname = "org-modern-indent";
    version = versionOf inputs.org-modern-indent;
    src = inputs.org-modern-indent;
  };

  panel = epkgs.trivialBuild {
    pname = "panel";
    version = versionOf inputs.panel;
    src = inputs.panel;
    packageRequires = [
      epkgs.async
      epkgs.nerd-icons
    ];
  };

  setup = epkgs.trivialBuild {
    pname = "setup";
    version = versionOf inputs.setup-el;
    src = inputs.setup-el;
  };

  lsp-proxy = epkgs.melpaBuild {
    pname = "lsp-proxy";
    version = versionOf inputs.lsp-proxy;
    src = inputs.lsp-proxy;
    # Patch lsp-proxy-core.el to use the Nix-built binary
    postPatch = ''
      substituteInPlace lsp-proxy-core.el \
        --replace-fail \
        "(executable-find \"emacs-lsp-proxy\")" \
        "\"${emacs-lsp-proxy-binary}/bin/emacs-lsp-proxy\""
    '';
    packageRequires = [
      epkgs.s
      epkgs.eldoc
      epkgs.ht
      epkgs.dash
      epkgs.f
      epkgs.yasnippet
    ];
    recipe = pkgs.writeText "lsp-proxy-recipe" ''
      (lsp-proxy :repo "jadestrong/lsp-proxy"
                 :fetcher github
                 :files ("*.el"))
    '';
  };

  symbol-overlay = epkgs.trivialBuild {
    pname = "symbol-overlay";
    version = versionOf inputs.symbol-overlay;
    src = inputs.symbol-overlay;
  };

  miniline = epkgs.trivialBuild {
    pname = "miniline";
    version = versionOf inputs.miniline;
    src = inputs.miniline;
    packageRequires = [ epkgs.nerd-icons ];
  };

  blame-reveal = epkgs.trivialBuild {
    pname = "blame-reveal";
    version = versionOf inputs.blame-reveal;
    src = inputs.blame-reveal;
  };

  eca = epkgs.melpaBuild {
    pname = "eca";
    version = versionOf inputs.eca;
    src = inputs.eca;
    packageRequires = [
      epkgs.compat
      epkgs.dash
      epkgs.f
      epkgs.markdown-mode
      epkgs.s
    ];
    recipe = pkgs.writeText "eca-recipe" ''
      (eca :repo "editor-code-assistant/eca-emacs"
           :fetcher github
           :files ("*.el"))
    '';
  };

  telega =
    let
      version = versionOf inputs.telega;
    in
    epkgs.melpaBuild {
      pname = "telega";
      inherit version;
      src = inputs.telega;
      packageRequires = [ epkgs.visual-fill-column ];
      buildInputs = [
        tdlib-head
        pkgs.zlib
      ];
      nativeBuildInputs = [
        pkgs.gnumake
        pkgs.gcc
        pkgs.pkg-config
      ];
      postPatch = ''
        substituteInPlace telega-customize.el \
          --replace-fail '(defcustom telega-server-libs-prefix "/usr/local"' \
                         '(defcustom telega-server-libs-prefix "${tdlib-head}"'
        substituteInPlace telega-customize.el \
          --replace-fail '(defcustom telega-server-command "telega-server"' \
                         "(defcustom telega-server-command \"$out/share/emacs/site-lisp/elpa/telega-${version}/telega-server\""
      '';
      preBuild = ''
        make -C server clean
        make -C server install \
          LIBS_PREFIX=${tdlib-head} \
          INSTALL_PREFIX=$out/share/emacs/site-lisp/elpa/telega-${version}
      '';
      recipe = pkgs.writeText "telega-recipe" ''
        (telega :repo "LuciusChen/telega.el"
                :fetcher github
                :files (:defaults "Makefile" "etc" "server" "contrib"))
      '';
    };
}
