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
      sdam-git =
        pkgs.fetchFromGitHub {
          owner = "int-index";
          repo = "sdam";
          rev = "3943cd2662eff11e1a36bc629ac832652ec43a6e";
          sha256 = "05p43wvmx41rlnj5nwlcngr8rxhms5dxlh210ag2r04gxwra2mrv";
        };
      slay-git =
        pkgs.fetchFromGitHub {
          owner = "int-index";
          repo = "slay";
          rev = "1c9d39b8cb4f32f0b4778677c21ebb85cc1cddf7";
          sha256 = "0x9xqaykdw5z3ggi7mkm7f5605c7z4jalhydvf9p1asdi5i34f8j";
        };
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
          inj-base = pkgs.haskell.lib.appendPatch (hself.callHackage "inj-base" "0.2.0.0" {}) ./patches/inj-base.patch;
          morte = pkgs.haskell.lib.doJailbreak (hself.callCabal2nix "morte" "${morte-git}" {});
          lrucaching = pkgs.haskell.lib.doJailbreak (hself.callHackage "lrucaching" "0.3.3" {});
          ListLike = pkgs.haskell.lib.dontCheck hsuper.ListLike;
          sdam = hself.callCabal2nix "sdam" sdam-git {};
          slay-core = hself.callCabal2nix "slay-core" "${slay-git}/core" {};
          slay-combinators = hself.callCabal2nix "slay-combinators" "${slay-git}/combinators" {};
          slay-cairo = hself.callCabal2nix "slay-cairo" "${slay-git}/cairo" {};
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
