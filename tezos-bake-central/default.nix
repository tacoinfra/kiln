{ system ? builtins.currentSystem }:
let
  obelisk = import .obelisk/impl { inherit system; };
in
obelisk.project ./. ({ pkgs, ... }:
  let
    reflex-platform = obelisk.reflex-platform;
    rhyolite-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "rhyolite";
      rev = "173e9bcc7dad469ea9b4110243c829b0f1dd6d9c";
      sha256 = "0khf21w0rap4isqdxaziby6rj79yrriig72qfnc051ayc9ql81nb";
    };

    gargoyle-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "72355581ef1f8663e2772b31b90dc074d32a93a1";
      sha256 = "08w7aa5mb49ypqi8nhlrpd8jcm10h4ja9xsmb8b8mvcch6sk1ykf";
    };
    groundhog-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "groundhog";
      rev = "c2f18be45e3233f6268c8468eb0732dd6b2e8009";
      sha256 = "1r9i78bsnm6idbvp87gjklnr10g7c83nsbnrffkyrn1wmd7zzqdn";
    };
  in {
    packages = {
      groundhog = groundhog-src + /groundhog;
      groundhog-postgresql = groundhog-src + /groundhog-postgresql;
      groundhog-th = groundhog-src + /groundhog-th;

      reflex-aeson-orphans = pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-aeson-orphans";
        rev = "064163c69725d6dc82e76f2b7c5cbf3543405e02";
        sha256 = "1gdpmw1323gwn4sfgd8ilbjj5pswnxq1h0h31babnnsif51d3yh2";
      };

      reflex-dom-forms = pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-forms";
        rev = "2f7c4a8f80d464f4c0289dfb5aae0c204827592e";
        sha256 = "0s32099nyk7pw44w9nsgi6q1w16f81sd57snc9gj131ymjp9nprv";
      };

      # rhyolite-backend needs a custom dependency injection
      rhyolite-backend-snap = rhyolite-src + /backend-snap;
      rhyolite-common = rhyolite-src + /common;
      rhyolite-frontend = rhyolite-src + /frontend;
    };
    overrides = self: super: {
      tezos-bake-monitor-lib = pkgs.haskell.lib.dontHaddock (
        self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {});

      gargoyle = (self.callCabal2nix "gargoyle" (gargoyle-src + /gargoyle) {});
      gargoyle-postgresql = (self.callCabal2nix "gargoyle-postgresql" (gargoyle-src + /gargoyle-postgresql) {});
      gargoyle-postgresql-nix = pkgs.haskell.lib.addBuildTools
        (self.callCabal2nix "gargoyle-postgresql-nix" (gargoyle-src + /gargoyle-postgresql-nix) {})
        [ pkgs.postgresql ]; # TH use of `staticWhich` for `psql` requires this on the PATH during build time.

      rhyolite-backend = self.callCabal2nix "rhyolite-backend" (rhyolite-src + /backend) { websockets = self.websockets-obsidian; };

      semantic-reflex = pkgs.haskell.lib.dontCheck (self.callCabal2nix "semantic-reflex" (pkgs.fetchFromGitHub {
        owner = "tomsmalley";
        repo = "semantic-reflex";
        rev = "38fce7e4d08d46b8664768f1b7fe38846dbac1e2";
        sha256 = "1s2p12r682wd8j2z63pjvbi4s9v02crh6nz8kjilwdsfs02yp5p2";
      } + /semantic-reflex) {});

      websockets-obsidian = self.callCabal2nix "websockets-obsidian" (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "websockets";
        rev = "62954d82401a9a2304a14a49973bd8c33db6a8f2";
        sha256 = "1cglrx6pbl5mdgfcsds5w2y1s4i9j375b3xim33jydr5g6c9ss4z";
      }) {};
      websockets-snap = self.callCabal2nix "websockets-snap" (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "websockets-snap";
        rev = "0587aaeab9f9005d45b221c8ccc08b42dde9f900";
        sha256 = "0s07f9sdn98h88kxkv8jr455a559c43c8ybdyvbv5c94ipbz7pjj";
      }) { websockets = self.websockets-obsidian; };

      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};

      heist = pkgs.haskell.lib.doJailbreak super.heist; # allow heist to use newer version of aeson
    };
})
