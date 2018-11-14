{ pkgs ? import <nixpkgs> {},
  hc ? "ghc844"
}:

pkgs.stdenv.mkDerivation rec {
  name = "foundry";
  buildInputs = [
    pkgs.haskell.compiler.${hc}
    pkgs.cabal-install
    pkgs.git
    pkgs.zlib
    pkgs.pkgconfig
    pkgs.cairo
    pkgs.pango
    pkgs.gtk3
    pkgs.which # for CI
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
  LOCALE_ARCHIVE =
    if pkgs.stdenv.isLinux
    then "${pkgs.glibcLocales}/lib/locale/locale-archive"
    else "";
}
