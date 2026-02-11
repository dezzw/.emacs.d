{
  description = "Demacs package system: MELPA/ELPA/Nongnu + custom packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # =========================================================================
    # Custom elisp packages
    # =========================================================================
    eglot-x = {
      url = "github:nemethf/eglot-x";
      flake = false;
    };
    emt = {
      url = "github:roife/emt";
      flake = false;
    };
    image-slicing = {
      url = "github:ginqi7/image-slicing";
      flake = false;
    };
    leetcode-emacs = {
      url = "github:ginqi7/leetcode-emacs";
      flake = false;
    };
    org-modern-indent = {
      url = "github:jdtsmith/org-modern-indent";
      flake = false;
    };
    panel = {
      url = "github:LuciusChen/panel";
      flake = false;
    };
    agent-shell = {
      url = "github:xenodium/agent-shell";
      flake = false;
    };
    agent-shell-sidebar = {
      url = "github:cmacrae/agent-shell-sidebar";
      flake = false;
    };
    setup-el = {
      url = "git+https://codeberg.org/pkal/setup.el.git";
      flake = false;
    };
    telega = {
      url = "github:LuciusChen/telega.el";
      flake = false;
    };
    blame-reveal = {
      url = "github:LuciusChen/blame-reveal";
      flake = false;
    };
    lsp-proxy = {
      url = "github:jadestrong/lsp-proxy";
      flake = false;
    };
    symbol-overlay = {
      url = "github:roife/symbol-overlay";
      flake = false;
    };
    consult-ripfd = {
      url = "github:jdtsmith/consult-ripfd";
      flake = false;
    };
    miniline = {
      url = "github:dezzw/miniline.el";
      flake = false;
    };
    eca = {
      url = "github:editor-code-assistant/eca-emacs";
      flake = false;
    };

    # =========================================================================
    # Non-elisp dependencies
    # =========================================================================
    tdlib = {
      url = "github:tdlib/td";
      flake = false;
    };
    tree-sitter-clojure = {
      url = "github:sogaiu/tree-sitter-clojure?ref=unstable-20250526";
      flake = false;
    };
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    {
      overlays.default = import ./overlay.nix { inherit inputs; };
    };
}
