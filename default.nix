let

  nixpkgsPin = import ./nix/nixpkgs-pin.nix;

in

{ pkgs ? import (builtins.fetchTarball nixpkgsPin) {},
  hc ? "ghc865"
}:

let

  sdam_from_github =
    pkgs.fetchFromGitHub {
      owner = "int-index";
      repo = "sdam";
      rev = "9e79a6f589921a8a0e05119dcf5711af2dddbca2";
      sha256 = "0g8kam2fg99frdc27snqpgphc1w0bag3jrxivb6qkxk8pg114zgq";
    };

  slay_from_github =
    pkgs.fetchFromGitHub {
      owner = "int-index";
      repo = "slay";
      rev = "69b533004637319c2f210dfb17f44acfce3f59d2";
      sha256 = "1bdh7p81a1w4y8c4rmq3knwjm10zxclx5lif4bs407ahfrsfi600";
    };

  morte_1_7_2 =
    pkgs.fetchFromGitHub {
      owner = "Gabriel439";
      repo = "Haskell-Morte-Library";
      rev = "f7a459c7ec1d69f71c389f5693a26a94c1854636";
      sha256 = "186mysp5f3mpd2mzgba2rfs5hlccf5whdi4gcr317zk82aqlzwrz";
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
        sdam = self.callCabal2nix "sdam" sdam_from_github {};
        slay-core = self.callCabal2nix "slay-core" "${slay_from_github}/core" {};
        slay-combinators = self.callCabal2nix "slay-combinators" "${slay_from_github}/combinators" {};
        slay-cairo = self.callCabal2nix "slay-cairo" "${slay_from_github}/cairo" {};
        morte = self.callCabal2nix "morte" morte_1_7_2 {};
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
        exe:foundry exe:morte-to-sdam exe:hask exe:haskell-to-sdam
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
