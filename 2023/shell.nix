# https://discourse.nixos.org/t/nix-haskell-development-2020/6170
# https://discourse.nixos.org/t/super-simple-haskell-development-with-nix/14287/3
{ pkgs ? import <nixpkgs> {} }:

let
  ghcEnv = pkgs.haskell.packages.ghc94.ghcWithPackages (
    ps: with ps; [
      vector
      hmatrix
    ]
  );
in pkgs.mkShellNoCC {
  name = "advent-of-code";
  buildInputs = with pkgs; [ ghcEnv hlint ormolu python3 ];
}
