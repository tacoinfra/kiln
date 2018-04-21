{ pkgs ? import <nixpkgs> {} }:
  let
    dontCheck = pkgs.haskell.lib.dontCheck;
    haskellPackages = pkgs.haskellPackages.override
    {
      overrides = self: super:
        {
          heist = dontCheck super.heist;
        };
    };
    inherit (haskellPackages) cabal cabal-install text aeson snap;
  in haskellPackages.callPackage (import ./tezos-bake-monitor.nix) {}
