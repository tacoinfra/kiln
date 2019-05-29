{ system ? builtins.currentSystem
, supportGargoyle ? true  # This must default to `true` for 'ob run' to work.
, profiling ? false
, distMethod ? null
, tezosScopedKit ? null
}:
let
  obelisk = import .obelisk/impl { inherit system profiling; };
in
obelisk.project ./. ({ pkgs, ... }@args:
  let
    inherit (obelisk.reflex-platform) hackGet;
    rhyolite = import (hackGet dep/rhyolite);
    nodeKit = if tezosScopedKit != null then tezosScopedKit else import ./scoped-tzkits.nix {
      inherit pkgs;
      tezos-baking-platform = import (hackGet ../dep/tezos-baking-platform) {};
    };
  in {
    staticFiles = pkgs.callPackage ./static { pkgs = obelisk.nixpkgs; };
    # staticFilesImpure = toString ./result-static;
    packages = {
      backend-db = ./backend-db;
      tezos-bake-monitor-lib = ../tezos-bake-monitor-lib;
      tezos-noderpc = ../tezos-noderpc;

      # Obelisk thunks. Place here so can repl and build locally when unpacked.
      dependent-sum-template = hackGet dep/dependent-sum-template;
      functor-infix = hackGet dep/functor-infix;
      micro-ecc = hackGet ../dep/micro-ecc-haskell;
      named = hackGet dep/named; # TODO: Drop once package set includes 0.3.0.0
      reflex-dom-forms = hackGet dep/reflex-dom-forms;
      semantic-reflex = hackGet dep/semantic-reflex + "/semantic-reflex";
    };

    overrides = pkgs.lib.composeExtensions (rhyolite args).haskellOverrides (self: super: with pkgs.haskell.lib; {
      common = if distMethod == null
        then super.common
        else enableCabalFlag super.common distMethod;
      backend = overrideCabal super.backend (drv:{
        librarySystemDepends = drv.librarySystemDepends or [] ++ [nodeKit];
      });
      backend-db = if supportGargoyle
        then
          enableCabalFlag (addBuildDepend super.backend-db self.rhyolite-backend-db-gargoyle) "support-gargoyle"
        else
          super.backend-db;
      base58-bytestring = dontCheck super.base58-bytestring; # disable tests for GHCJS build
      email-validate = dontCheck super.email-validate; # disable tests for GHCJS build
      extra = dontCheck super.extra; # disable unreliable tests (https://github.com/ndmitchell/extra/issues/37)
      semantic-reflex = dontHaddock (dontCheck super.semantic-reflex);
      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};
      tezos-bake-monitor-lib = dontHaddock super.tezos-bake-monitor-lib;
      tezos-noderpc = dontHaddock super.tezos-noderpc;
    });
  })
