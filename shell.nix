{ _nixpkgs ? import <nixpkgs> {} }:

let
  nixpkgs = import (_nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "e7a327da5cffdf5e77e1924906a4f0983591bd3e";
    sha256 = "1xzil4mayhggg2miwspbk12nihlszg0y4n6i4qacrxql5n75f0hr";
  }){ overlays = [cabalHashes]; };

  cabalHashes = sel: super: {
    all-cabal-hashes = super.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b2b93ae610f5f1b51d22b191f972dc3dec8f94c6.tar.gz";
      sha256 = "0bffclpqbw62xff36qlzxghr042mhv0m06k5ml4298w6fv7ly1xw";
    };
  };

  ghc = nixpkgs.haskell.compiler.ghc802;
  haskellPackages = nixpkgs.haskell.packages.ghc802;

in

nixpkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "foundry-env";
  buildInputs = with nixpkgs;
    [ git
      gmp
      haskellPackages.cpphs
      haskellPackages.happy
      haskellPackages.gtk2hs-buildtools
      lzma
      gtk3
      zlib
    ];
  buildPhase = ''
    export LANG=en_US.UTF-8
    '';
    }
