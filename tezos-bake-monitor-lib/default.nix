{ pkgs ? (import <nixpkgs> {}).pkgs }:
  let
    dontCheck = pkgs.haskell.lib.dontCheck;
    haskellPackages = pkgs.haskellPackages.override
    {
      overrides = self: super:
        {
          tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ./. {};
          heist = dontCheck super.heist;
        };
    };
    inherit (haskellPackages) cabal cabal-install text aeson bytestring;
  in haskellPackages.tezos-bake-monitor-lib
