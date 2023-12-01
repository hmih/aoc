# https://discourse.nixos.org/t/nix-haskell-development-2020/6170
# https://discourse.nixos.org/t/super-simple-haskell-development-with-nix/14287/3
{ pkgs ? import <nixpkgs> {} }:

# shell
#let
#  ghcEnv = pkgs.haskell.packages.ghc94.ghcWithPackages (
#    ps: with ps; [
#      vector
#      hmatrix
#    ]
#  );
#in pkgs.stdenv.mkDerivation {
#  name = "advent-of-code";
#  #buildInputs = [ ghcEnv pkgs.hlint pkgs.idris2 ];
#  buildInputs = [ zsh pkgs.idris2 ];
#}
pkgs.mkShellNoCC {
  name = "advent-of-code";
  buildInputs = with pkgs; [ zsh idris2 racket-minimal ];
}
