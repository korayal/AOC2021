let sources = import ./nix/sources.nix { };

in { ghc ? "ghc8107", pkgs ? import sources.nixpkgs {
  config = {
    allowBroken = true;
    allowUnfree = true;
  };
} }:
with pkgs;
let

  inherit (pkgs) callPackage;
  inherit (pkgs.lib) composeExtensions sourceByRegex;
  inherit (pkgs.haskell.lib) dontCheck justStaticExecutables;

  aoc2021Overlay = _self: super: {
    aoc2021 = dontCheck (justStaticExecutables (super.callCabal2nix "aoc2021"
      (sourceByRegex ./. [
        "^src.*$"
        "^.*\\.cabal$"
        "package.yaml"
        "Build.hs"
        "^LICENSE$"
      ]) { }));

  };

  ############################################################################
  # Construct a 'base' Haskell package set
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};

  # Makes overlays given a base haskell package set. Can be used by
  # other projects
  haskellOverlays = [ aoc2021Overlay ];

  # Construct the final Haskell package set
  haskellPkgs = baseHaskellPkgs.override (old: {
    overrides = builtins.foldl' composeExtensions (old.overrides or (_: _: { }))
      haskellOverlays;
  });
in {
  inherit haskellPkgs;
  inherit pkgs;
  inherit sources;
  inherit haskellOverlays;
}
