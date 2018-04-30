{ pkgs }:
  let
    dontCheck = pkgs.haskell.lib.dontCheck;
    haskellPackages = pkgs.haskellPackages.override
    {
      overrides = self: super:
        {
          tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {};
          tezos-bake-monitor = self.callCabal2nix "tezos-bake-monitor" ./. {};
          heist = dontCheck super.heist;
        };
    };
    inherit (haskellPackages) cabal cabal-install text aeson snap safe async optparse-applicative http-client http-client-tls http-types bytestring;
  in haskellPackages.tezos-bake-monitor
