{ pkgs ? (import <nixpkgs> {}) }:
  let
    dontCheck = pkgs.haskell.lib.dontCheck;
    haskellPackages = pkgs.haskellPackages.override
    {
      overrides = self: super:
        {
          tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {};
          tezos-noderpc = self.callCabal2nix "tezos-noderpc" ./. {};
          heist = dontCheck super.heist;
        };
    };
    inherit (haskellPackages) cabal cabal-install text aeson bytestring;
  in haskellPackages.tezos-bake-monitor-lib
