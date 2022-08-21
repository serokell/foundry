let

  nixpkgsPin = import ./nix/nixpkgs-pin.nix;

in

{ pkgs ? import (builtins.fetchTarball nixpkgsPin) {},
  hc ? "ghc925"
}:

let

  sdam_from_github =
    pkgs.fetchFromGitHub {
      owner = "int-index";
      repo = "sdam";
      rev = "3943cd2662eff11e1a36bc629ac832652ec43a6e";
      sha256 = "05p43wvmx41rlnj5nwlcngr8rxhms5dxlh210ag2r04gxwra2mrv";
    };

  slay_from_github =
    pkgs.fetchFromGitHub {
      owner = "int-index";
      repo = "slay";
      rev = "1c9d39b8cb4f32f0b4778677c21ebb85cc1cddf7";
      sha256 = "0x9xqaykdw5z3ggi7mkm7f5605c7z4jalhydvf9p1asdi5i34f8j";
    };

  morte_from_github =
    pkgs.fetchFromGitHub {
      owner = "Gabriella439";
      repo = "Haskell-Morte-Library";
      rev = "b7ebbcbea21e3894b889ebd882856ffcdb154160";
      hash = "sha256-jO4EpCA+Xm7+oo0Xa8TIN+TX/bAjvQIcVYfQfbtAC5k=";
    };

  haskell_inputs = p: [
    p.gtk3
    p.lens
    p.dlist
    p.ghc-lib-parser
    p.megaparsec
    p.inj
    p.inj-base
    p.regex-applicative
    p.split
    p.streams
    p.sdam
    p.slay-core
    p.slay-combinators
    p.slay-cairo
    p.morte
  ];

  haskellPackages =
    pkgs.haskell.packages.${hc}.override {
      overrides = self: super: rec {
        inj-base = pkgs.haskell.lib.appendPatch (self.callHackage "inj-base" "0.2.0.0" {}) ./patches/inj-base.patch;
        morte = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "morte" "${morte_from_github}" {});
        lrucaching = pkgs.haskell.lib.dontCheck (self.callHackage "lrucaching" "0.3.3" {});
        ListLike = pkgs.haskell.lib.dontCheck super.ListLike;
        sdam = self.callCabal2nix "sdam" sdam_from_github {};
        slay-core = self.callCabal2nix "slay-core" "${slay_from_github}/core" {};
        slay-combinators = self.callCabal2nix "slay-combinators" "${slay_from_github}/combinators" {};
        slay-cairo = self.callCabal2nix "slay-cairo" "${slay_from_github}/cairo" {};
      };
    };

in

pkgs.stdenv.mkDerivation rec {
  name = "foundry";
  src = ./.;
  buildCommand = ''
    mkdir -p $out/bin $out/home
    cp -r $src/foundry.cabal $src/src .
    HOME=$out/home \
      cabal --offline --config-file /dev/null v2-install \
        --builddir=$out/dist \
        --installdir=$out/bin \
        exe:foundry exe:morte-to-sdam exe:hask exe:haskell-to-sdam exe:sdam-to-svg
  '';
  buildInputs = [
    (haskellPackages.ghcWithPackages haskell_inputs)
    pkgs.cabal-install
    pkgs.git
    pkgs.zlib
    pkgs.pkgconfig
    pkgs.cairo
    pkgs.pango
    pkgs.gtk3
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
