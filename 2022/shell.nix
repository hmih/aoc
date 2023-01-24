# https://discourse.nixos.org/t/nix-haskell-development-2020/6170
# https://discourse.nixos.org/t/super-simple-haskell-development-with-nix/14287/3

# shell
let
  nixpkgs = builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/c69ebebea7ac954ec70bbe8334dbe2595e85a666.tar.gz;
    sha256 = "0y4ca4mk2vs4x8rlgq18v23i6ygnq2lh3x9cvpv0mkbq93lfd9q9";
  };
  pkgs = import nixpkgs {
    config = {};   # potentially override the system config here
    overlays = []; # <- set your overlays here
  };
  ghcEnv = pkgs.haskell.packages.ghc924.ghcWithPackages (
    ps: with ps; [
      vector
      text
    ]
  );
in
pkgs.stdenv.mkDerivation {
  name = "advent-of-code";
  buildInputs = [ ghcEnv pkgs.hlint ];
  shellHook = "eval $(egrep ^export ${ghcEnv}/bin/ghc)";
}
