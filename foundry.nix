{ linux ? false, linux-static ? false, windows ? false }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  pkgs = if linux-static then nixpkgs.pkgsCross.musl64 else if windows then nixpkgs.pkgsCross.mingwW64 else nixpkgs;
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.slay = {
        configureFlags = with pkgs;
        if linux-static then [
          "--ghc-option=-optl=-L${zlib.static}/lib"
        ] else [
          "--ghc-option=-optl=-L${zlib}/lib"
        ];
      };
      packages.foundry = {
        package.ghcOptions = "-Werror";
        configureFlags = with pkgs;
        lib.optionals linux-static [
          "--ghc-option=-optl=-L${zlib.static}/lib"
        ];
      };
    }];
  };
in project.foundry
