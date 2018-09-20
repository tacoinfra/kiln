{ system ? builtins.currentSystem
, supportGargoyle ? true  # This must default to `true` for 'ob run' to work.
}:
let
  obelisk = import .obelisk/impl { inherit system; };
in
obelisk.project ./. ({ pkgs, ... }@args:
  let
    rhyolite-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "rhyolite";
      rev = "3ac3e40edec5354b773af55c142427714ce2ef71";
      sha256 = "1d75r3hx6bbh3kgki1j6l3rldn2awgcmxwc878gi0rgpmv3gdynx";
    };
    rhyoliteLib = args: (import rhyolite-src).lib args;

  in {
    packages = {
      backend-db = ./backend-db;
      reflex-dom-forms = pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-forms";
        rev = "2f7c4a8f80d464f4c0289dfb5aae0c204827592e";
        sha256 = "0s32099nyk7pw44w9nsgi6q1w16f81sd57snc9gj131ymjp9nprv";
      };
      tezos-bake-monitor-lib = ../tezos-bake-monitor-lib;
    };

    overrides = pkgs.lib.composeExtensions (rhyoliteLib args).haskellOverrides (self: super: with pkgs.haskell.lib; {
      email-validate = dontCheck super.email-validate;
      modern-uri = dontCheck super.modern-uri;
      base58-bytestring = dontCheck super.base58-bytestring;
      lens-aeson = dontCheck super.lens-aeson;
      megaparsec = dontCheck super.megaparsec;
      backend-db = if supportGargoyle
        then
          pkgs.haskell.lib.enableCabalFlag (pkgs.haskell.lib.addBuildDepend super.backend-db self.rhyolite-backend-db-gargoyle) "support-gargoyle"
        else
          super.backend-db;

      semantic-reflex = pkgs.haskell.lib.dontCheck (self.callCabal2nix "semantic-reflex" (pkgs.fetchFromGitHub {
        owner = "tomsmalley";
        repo = "semantic-reflex";
        rev = "38fce7e4d08d46b8664768f1b7fe38846dbac1e2";
        sha256 = "1s2p12r682wd8j2z63pjvbi4s9v02crh6nz8kjilwdsfs02yp5p2";
      } + /semantic-reflex) {});

      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};

      tezos-bake-monitor-lib = pkgs.haskell.lib.dontHaddock (super.tezos-bake-monitor-lib);
    });
  })
