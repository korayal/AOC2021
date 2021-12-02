with (import ./. { });
let
  hls-nix = (import sources.hls-nix { }).haskell-language-server;
  pre-commit-hooks = import sources."pre-commit-hooks.nix";
  pre-commit-config = {
    src = ./.;
    tools = pkgs;
    settings = { hpack = { silent = true; }; };
    hooks = {
      yamllint.enable = true;
      hpack.enable = true;
      nixfmt = {
        enable = true;
        excludes = [ "nix/default.nix" ];
      };
    };
  };

  pre-commit-check = pre-commit-hooks.run pre-commit-config;

  inherit (pkgs.haskell.lib) doBenchmark doCheck;

in haskellPkgs.shellFor {
  packages = p: [ p.aoc2021 ];

  withHoogle = true;

  doBenchmark = true;

  nativeBuildInputs = with pkgs; [
    cabal-install
    ghcid
    hlint
    hpack
    ormolu
    hls-nix
  ];

  shellHook = ''
    { ${pre-commit-check.shellHook} } 2> /dev/null
  '';
}
