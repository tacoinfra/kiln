{ system ? builtins.currentSystem
, supportGargoyle ? true  # This must default to `true` for 'ob run' to work.
, profiling ? false
}:
let
  obelisk = import .obelisk/impl { inherit system profiling; };
in
obelisk.project ./. ({ pkgs, ... }@args:
  let
    inherit (obelisk.reflex-platform) hackGet;
    rhyolite-src = hackGet dep/rhyolite;
    rhyoliteLib = args: (import rhyolite-src).lib args;
  in {
    staticFiles = pkgs.callPackage ./static { pkgs = obelisk.nixpkgs; };
    staticFilesImpure = toString ./result-static;
    packages = {
      backend-db = ./backend-db;
      tezos-bake-monitor-lib = ../tezos-bake-monitor-lib;
      tezos-noderpc = ../tezos-noderpc;

      # Obelisk thunks. Place here so can repl and build locally when unpacked.
      dependent-sum-aeson-orphans = hackGet dep/dependent-sum-aeson-orphans;
      functor-infix = hackGet dep/functor-infix;
      reflex-dom-forms = hackGet dep/reflex-dom-forms;
      semantic-reflex = hackGet dep/semantic-reflex + "/semantic-reflex";
    };

    overrides = pkgs.lib.composeExtensions (rhyoliteLib args).haskellOverrides (self: super: with pkgs.haskell.lib; {
      backend-db = if supportGargoyle
        then
          enableCabalFlag (addBuildDepend super.backend-db self.rhyolite-backend-db-gargoyle) "support-gargoyle"
        else
          super.backend-db;
      base58-bytestring = dontCheck super.base58-bytestring; # disable tests for GHCJS build
      email-validate = dontCheck super.email-validate; # disable tests for GHCJS build
      semantic-reflex = dontHaddock (dontCheck super.semantic-reflex);
      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};
      tezos-bake-monitor-lib = dontHaddock super.tezos-bake-monitor-lib;
      tezos-noderpc = dontHaddock super.tezos-noderpc;

      # Must be here because it affects upstream rhyolite build
      constraints-extras = self.callCabal2nix "constraints-extras" (hackGet dep/constraints-extras) {};
    });
  })
