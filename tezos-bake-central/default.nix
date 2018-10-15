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
      rev = "be2dd0488072342a25675bec71aba1d9c7acae99";
      sha256 = "1q8pnp5cl77zc0avbw3mv38si43m3xps0g8whm6cjplg0p2a4ih6";
    };
    rhyoliteLib = args: (import rhyolite-src).lib args;
    semantic-reflex-src = pkgs.fetchFromGitHub {
      owner = "tomsmalley";
      repo = "semantic-reflex";
      rev = "42bfede5e308bab4494e87ed0144f21134a4c5b3";
      sha256 = "01rpf0vh5llx1hq4j55gmw36fvzhb95ngcykh34sgcxp5498p9f3";
    };
  in {
    staticFiles = pkgs.callPackage ./static {};
    packages = {
      backend-db = ./backend-db;
      reflex-dom-forms = pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-forms";
        rev = "2f7c4a8f80d464f4c0289dfb5aae0c204827592e";
        sha256 = "0s32099nyk7pw44w9nsgi6q1w16f81sd57snc9gj131ymjp9nprv";
      };
      tezos-bake-monitor-lib = ../tezos-bake-monitor-lib;
      tezos-noderpc = ../tezos-noderpc;
    };

    overrides = pkgs.lib.composeExtensions (rhyoliteLib args).haskellOverrides (self: super: with pkgs.haskell.lib; {
      backend-db = if supportGargoyle
        then
          pkgs.haskell.lib.enableCabalFlag (pkgs.haskell.lib.addBuildDepend super.backend-db self.rhyolite-backend-db-gargoyle) "support-gargoyle"
        else
          super.backend-db;
      base58-bytestring = dontCheck super.base58-bytestring;
      email-validate = dontCheck super.email-validate;
      lens-aeson = dontCheck super.lens-aeson;
      megaparsec = dontCheck super.megaparsec;
      modern-uri = dontCheck super.modern-uri;
      semantic-reflex = dontHaddock (dontCheck (self.callCabal2nix "semantic-reflex" (semantic-reflex-src  + /semantic-reflex) {}));
      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};
      tezos-bake-monitor-lib = dontHaddock (super.tezos-bake-monitor-lib);
      tezos-noderpc = dontHaddock (super.tezos-noderpc);
    });
  })
