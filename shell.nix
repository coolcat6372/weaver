{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.chez
    pkgs.gnumake
    pkgs.chez-srfi
  ];
}
