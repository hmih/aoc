# https://discourse.nixos.org/t/nix-haskell-development-2020/6170
# https://discourse.nixos.org/t/super-simple-haskell-development-with-nix/14287/3
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShellNoCC {
  name = "advent-of-code";
  buildInputs = with pkgs; [ python3 ];
}
