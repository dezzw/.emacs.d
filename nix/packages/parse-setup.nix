# parse-setup.nix -- Parse setup.el forms from Emacs Lisp configuration files.
# Extracts package names and :pkg metadata for Nix package management.
#
# Usage:
#   let
#     parseSetup = import ./parse-setup.nix { inherit lib; };
#     pkgs = parseSetup.parsePackagesFromSetupFiles {
#       files = [ ./init.el ];
#       dirs  = [ ./modules ];
#     };
#   in ...
#
# Each returned record has:
#   { name = "pkg-name"; }                           -- auto-detected
#   { name = "pkg-name"; url = "https://..."; }      -- VC package
#   { name = "pkg-name"; feature = "setup-name"; }   -- feature != package
{ lib }:

let
  inherit (builtins)
    readDir readFile filter length head tail elemAt
    isList isString concatMap attrNames hasAttr elem;
  inherit (lib) hasSuffix filterAttrs;

  fromElispLib = import ./fromElisp.nix { inherit lib; };
  inherit (fromElispLib) fromElisp;

  # ── Built-in features blocklist ──────────────────────────────────────
  # Emacs features that ship with Emacs and have no separate MELPA/ELPA
  # package.  These are silently skipped during auto-detection.
  builtinFeatures = [
    # Core / C-level
    "emacs" "window" "frame" "faces" "custom" "hl-line" "paren"
    "indent" "mouse" "simple" "hideshow" "imenu" "files" "dired"
    "bookmark" "recentf" "minibuffer" "isearch" "autorevert"
    "compile" "flymake" "tramp" "project" "xref" "treesit"
    # Built-in language modes
    "sql" "python" "go" "lisp-mode" "js-ts-mode"
    # Built-in Org sub-features
    "org" "ob-core" "org-agenda" "org-habit" "ox-latex"
    # Built-in display features
    "display-fill-column-indicator" "display-line-numbers"
    "tab-bar" "tab-line" "which-key"
    # Built-in misc
    "window-divider" "ediff-wind" "mwheel" "transient" "eglot"
    "auth-sources" "esh-mode"
    # Sub-features of packages (not separate packages)
    "magit-log" "org-remark-nov"
  ];

  isBuiltin = name: elem name builtinFeatures;

  # ── Plist helpers ────────────────────────────────────────────────────
  # Find a value by keyword key in a flat plist-like list.
  # e.g. findInPlist ":url" [":url" "https://..."] => "https://..."
  findInPlist = key: lst:
    if lst == [] || length lst < 2 then null
    else if head lst == key then elemAt lst 1
    else findInPlist key (tail (tail lst));

  # ── :pkg form finder ─────────────────────────────────────────────────
  # Search a list of body forms for the first (:pkg ...) sublist.
  findPkgForm = body:
    let
      pkgForms = filter (item:
        isList item && item != [] && head item == ":pkg"
      ) body;
    in
      if pkgForms == [] then null
      else head pkgForms;

  # ── :pkg form parser ─────────────────────────────────────────────────
  # Parse a :pkg form and return { name; url?; feature?; }.
  #
  # Handles all 4 variants:
  #   [":pkg"]                           -> infer name from setupName
  #   [":pkg" "meow"]                    -> explicit name
  #   [":pkg" ["meow" ":url" "..."]]     -> explicit name + URL
  #   [":pkg" [":url" "..."]]            -> infer name + URL
  parsePkgForm = setupName: form:
    let
      args = tail form;  # drop ":pkg"
    in
      if args == [] then
        # (:pkg) -> infer from setup name
        { name = setupName; }
      else
        let arg = head args; in
        if isString arg then
          # (:pkg meow) -> explicit name
          { name = arg; }
          // (if arg != setupName then { feature = setupName; } else {})
        else if isList arg && arg != [] then
          let first = head arg; in
          if isString first && builtins.substring 0 1 first == ":" then
            # (:pkg (:url "...")) -> keywords only, infer name
            let url = findInPlist ":url" arg; in
            { name = setupName; }
            // (if url != null then { inherit url; } else {})
          else if isString first then
            # (:pkg (name :url "...")) -> explicit name + keywords
            let
              rest = tail arg;
              url = findInPlist ":url" rest;
            in
            { name = first; }
            // (if first != setupName then { feature = setupName; } else {})
            // (if url != null then { inherit url; } else {})
          else
            { name = setupName; }
        else
          { name = setupName; };

  # ── Setup name extractor ─────────────────────────────────────────────
  # Extract the feature/package name from the first argument of a
  # (setup ...) form.  Returns null for forms we cannot interpret.
  getSetupName = firstArg:
    if isString firstArg then firstArg
    else if isList firstArg && firstArg != [] then
      let first = head firstArg; in
      if first == ":require" && length firstArg >= 2 then
        let second = elemAt firstArg 1; in
        if isString second then second else null
      else if first == ":with-feature" && length firstArg >= 2 then
        let second = elemAt firstArg 1; in
        if isString second then second else null
      else if first == ":pkg" then
        # (setup (:pkg meow) ...) -- shorthand
        if length firstArg >= 2 then
          let second = elemAt firstArg 1; in
          if isString second then second
          else if isList second && second != [] then
            let inner = head second; in
            if isString inner && builtins.substring 0 1 inner != ":"
            then inner
            else null
          else null
        else null
      else null
    else null;

  # ── Setup form parser ─────────────────────────────────────────────────
  # Parse a single (setup ...) form into a package record or null.
  parseSetupForm = form:
    if length form < 2 then null
    else
      let
        rest = tail form;       # everything after "setup"
        firstArg = head rest;
        setupName = getSetupName firstArg;
        body = tail rest;       # everything after the first arg
        bodyLists = filter isList body;
        pkgForm = findPkgForm bodyLists;
      in
        if setupName == null then null
        # If :pkg is present, always process (even for built-in features
        # like esh-mode that need a separate package)
        else if pkgForm != null then
          parsePkgForm setupName pkgForm
        else if isBuiltin setupName then null
        else
          { name = setupName; };

  # ── Recursive form finder ────────────────────────────────────────────
  # Recursively walk the AST to find all (setup ...) forms, even when
  # nested inside (when ...), (progn ...), (if ...), etc.
  findSetupForms = form:
    if !isList form || form == [] then []
    else if head form == "setup" then
      let result = parseSetupForm form; in
      if result != null then [ result ] else []
    else
      concatMap findSetupForms (filter isList form);

  # ── File-level parser ────────────────────────────────────────────────
  # Uses tryEval because fromElisp's POSIX regex engine cannot handle
  # multi-byte UTF-8 character literals (e.g. ?○ in org config).
  # Files that fail to parse are silently skipped -- their packages
  # should go in the extraPackages list in flake.nix.
  parseFileText = text:
    let
      result = builtins.tryEval (fromElisp text);
      ast = if result.success then result.value else [];
    in
      concatMap findSetupForms ast;

  # ── Directory-level parser ───────────────────────────────────────────
  parseDir = dir:
    let
      entries = readDir dir;
      elFiles = filterAttrs
        (n: t: t == "regular" && hasSuffix ".el" n)
        entries;
    in
      concatMap
        (name: parseFileText (readFile (dir + "/${name}")))
        (attrNames elFiles);

  # ── Public API ───────────────────────────────────────────────────────
  # Parse packages from explicit file list and/or directory list.
  # Returns a deduplicated list of package records.
  parsePackagesFromSetupFiles = { files ? [], dirs ? [] }:
    let
      fileResults = concatMap (f: parseFileText (readFile f)) files;
      dirResults  = concatMap parseDir dirs;
      allResults  = fileResults ++ dirResults;
      # Deduplicate by name, preferring records with more metadata
      seen = builtins.foldl' (acc: pkg:
        if hasAttr pkg.name acc then
          let existing = acc.${pkg.name}; in
          # Prefer the record that has a url or feature attribute
          if hasAttr "url" pkg && !(hasAttr "url" existing)
          then acc // { ${pkg.name} = pkg; }
          else acc
        else acc // { ${pkg.name} = pkg; }
      ) {} allResults;
    in
      builtins.attrValues seen;

in
{
  inherit parsePackagesFromSetupFiles builtinFeatures;
}
