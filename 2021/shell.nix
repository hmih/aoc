# https://discourse.nixos.org/t/nix-haskell-development-2020/6170
# https://discourse.nixos.org/t/super-simple-haskell-development-with-nix/14287/3

# shell
let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {};
  ghcEnv = pkgs.haskell.packages.ghc921.ghcWithPackages (
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
