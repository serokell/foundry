# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "foundry";
  buildInputs = [
    haskell.compiler.ghc843
    pkgs.zlib
    pkgs.cabal-install
    pkgs.pkgconfig
    pkgs.cairo
    pkgs.pango
    pkgs.gtk3
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
  '';
}
