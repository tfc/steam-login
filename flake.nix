{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          name = "steam-login";
          root = ./.;
          buildTools = hp: {
            brittany = hp.brittany;
            cabal-fmt = hp.cabal-fmt;
          };
          hlintCheck.enable = true;
          hlsCheck.enable = true;
        };
      };
    };
}
