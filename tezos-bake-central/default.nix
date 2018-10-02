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
      rev = "42d37a40894a620d89d1241e144682698fdc5faa";
      sha256 = "08d4rqikiisg789a8wdv4s79kbryrr620qkgm8j95gdxapb27zn4";
    };
    rhyoliteLib = args: (import rhyolite-src).lib args;
    semantic-reflex-src = pkgs.fetchFromGitHub {
      owner = "danbornside";
      # owner = "tomsmalley";
      repo = "semantic-reflex";
      rev = "26268b0236679ef5f7e762f7bd84d00b86c02545";
      sha256 = "0g8ilbhrp4i4pfxkayh9cprdx1yffiyh7zw3qming61p9pcyvpa1";
    };
    reflex-src = pkgs.fetchFromGitHub {
      owner = "xplat";
      repo = "reflex";
      rev = "201970734c944a0cdb6654947f233c5bfe3e5bbb";
      sha256 = "10rx4ajfp20l33bpbw2dss6i3s8ck8ny5zm6b2ams0bs17f8w17b";
    };
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

      reflex = pkgs.haskell.lib.dontCheck (self.callCabal2nix "reflex" reflex-src {});

      semantic-reflex = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (self.callCabal2nix "semantic-reflex" (semantic-reflex-src  + /semantic-reflex) {}));

      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};

      tezos-bake-monitor-lib = pkgs.haskell.lib.dontHaddock (super.tezos-bake-monitor-lib);
    });
  })
