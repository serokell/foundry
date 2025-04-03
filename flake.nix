{
  description = "foundry - a structure for Morte";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      ghc = "ghc96";
      pkgs = nixpkgs.legacyPackages.${system};
      morte-git =
        pkgs.fetchFromGitHub {
          owner = "Gabriella439";
          repo = "Haskell-Morte-Library";
          rev = "b7ebbcbea21e3894b889ebd882856ffcdb154160";
          hash = "sha256-jO4EpCA+Xm7+oo0Xa8TIN+TX/bAjvQIcVYfQfbtAC5k=";
        };
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          foundry = hself.callCabal2nix "foundry" "${self}/src/" {};
          gi-gdk = pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gdk" "4.0.9" {});
          gi-gsk = pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gsk" "4.0.8" {});
          gi-gtk = pkgs.haskell.lib.dontCheck (hself.callHackage "gi-gtk" "4.0.11" {});
          morte = pkgs.haskell.lib.doJailbreak (hself.callCabal2nix "morte" "${morte-git}" {});
          lrucaching = pkgs.haskell.lib.doJailbreak (hself.callHackage "lrucaching" "0.3.4" {});
        });
    in
    {
      packages = {
        foundry = haskellPackages.foundry;
        default = self.packages.${system}.foundry;
      };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages(p:
            p.foundry.getCabalDeps.libraryHaskellDepends ++
            p.foundry.getCabalDeps.executableHaskellDepends
          ))
          haskellPackages.foundry.getCabalDeps.executableToolDepends
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
          haskellPackages.fast-tags
          haskellPackages.ormolu
        ];
      };
    });
}
