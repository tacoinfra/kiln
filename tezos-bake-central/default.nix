{ system ? builtins.currentSystem
, supportGargoyle ? true  # This must default to `true` for 'ob run' to work.
}:
let
  obelisk = import .obelisk/impl { inherit system; };
in
obelisk.project ./. ({ pkgs, ... }@args:
  let
    bytestring-trie-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "bytestring-trie";
      rev = "27117ef4f9f01f70904f6e8007d33785c4fe300b";
      sha256 = "103fqr710pddys3bqz4d17skgqmwiwrjksn2lbnc3w7s01kal98a";
    };
    rhyolite-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "rhyolite";
      rev = "212e9898d464378270cd1e8b58f6e262d43f0aa3";
      sha256 = "0lcvmzpmrg9d11h43f0v9pwkn6a0xi23j3kzq8hgfq3nhygjshs4";
    };
    rhyoliteLib = args: (import rhyolite-src).lib args;
    semantic-reflex-src = pkgs.fetchFromGitHub {
      owner = "danbornside";
      # owner = "tomsmalley";
      repo = "semantic-reflex";
      rev = "26268b0236679ef5f7e762f7bd84d00b86c02545";
      sha256 = "0g8ilbhrp4i4pfxkayh9cprdx1yffiyh7zw3qming61p9pcyvpa1";
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

      # TODO Don't jailbreak.
      gargoyle = pkgs.haskell.lib.doJailbreak super.gargoyle;
      gargoyle-postgresql = pkgs.haskell.lib.doJailbreak super.gargoyle-postgresql;
      gargoyle-nix = pkgs.haskell.lib.doJailbreak super.gargoyle-nix;

      bytestring-trie = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "bytestring-trie" bytestring-trie-src {});

      semantic-reflex = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (self.callCabal2nix "semantic-reflex" (semantic-reflex-src  + /semantic-reflex) {}));

      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};

      tezos-bake-monitor-lib = pkgs.haskell.lib.dontHaddock (super.tezos-bake-monitor-lib);
    });
  })
