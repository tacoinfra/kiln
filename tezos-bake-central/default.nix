{ system ? builtins.currentSystem }:
let
  obelisk = import .obelisk/impl { inherit system; };
in
obelisk.project ./. ({ pkgs, ... }:
  let
    # reflex-src = ../../../reflex;
    reflex-src = pkgs.fetchFromGitHub {
      owner = "danbornside";
      repo = "reflex";
      rev = "7ab5470aa19bc56db8c488571996330b4ab62e24";
      sha256 = "14k5ffxc9f0xqf39gl1mf0algzmajq0qhya059y751k2qbv0d0jv";
    };

    reflex-dom-src = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom";
      rev = "f02da6c7e071153d8fad989297690f97c80e851f";
      sha256 = "1hxkc2jwf7wxvrk5nq8w3y3xb48v5vxl35ykr57dhnrq00v8g6p9";
    };

    # rhyolite-src = ../../../rhyolite;
    rhyolite-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "rhyolite";
      rev = "074dcb9748ec1c1aadcc061becf13e8885752a2c";
      sha256 = "1q32fmqqk6zzyl1jnbirylqf18nr9x3am372nq977cz6rp6431w5";
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
    universe-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "universe";
      rev = "c3575f15dba30b4fd3d1097e2caa7428bd967e5d";
      sha256 = "0nryry4nqhfhdv577hi9wrk9rrjq5xwcf880ixdkq1jb5yrfm641";
    };
    # monoidal-containers-src = ../../../monoidal-containers;
    monoidal-containers-src = pkgs.fetchFromGitHub {
      owner = "danbornside";
      repo = "monoidal-containers";
      rev = "f9bbf89b0f59ebcccbf116beefb26ce6d416cc69";
      sha256 = "1pbhprjsg5yh9483hgspy32smv9mbrkh16zdgmpw0qm9vmlw95cx";
    };
  in {
    packages = {
      groundhog = groundhog-src + /groundhog;
      groundhog-postgresql = groundhog-src + /groundhog-postgresql;
      groundhog-th = groundhog-src + /groundhog-th;

      # reflex-aeson-orphans = ../../../reflex-aeson-orphans;
      reflex-aeson-orphans = pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-aeson-orphans";
        rev = "a0e376563ddaf440a9fdc803c6cc62713d1a4c3a";
        sha256 = "0d8d63yhbqc77sglwna3wp7j05hmhq0mlyri2bl9prchw3hbb2gr";
      };

      reflex-dom-forms = pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-forms";
        rev = "2f7c4a8f80d464f4c0289dfb5aae0c204827592e";
        sha256 = "0s32099nyk7pw44w9nsgi6q1w16f81sd57snc9gj131ymjp9nprv";
      };

      # rhyolite-backend needs a custom dependency injection
      rhyolite-aeson-orphans = rhyolite-src + /aeson-orphans;
      rhyolite-backend-db = rhyolite-src + /backend-db;
      rhyolite-backend-snap = rhyolite-src + /backend-snap;
      rhyolite-common = rhyolite-src + /common;
      rhyolite-datastructures = rhyolite-src + /datastructures;
      rhyolite-frontend = rhyolite-src + /frontend;

      constraints-extras = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "constraints-extras";
        rev =  "abd1bab0738463657fc6303e606015a97b01c8a0";
        sha256 = "0lpc3cy8a7h62zgqf214g5bf68dg8clwgh1fs8hada5af4ppxf0l";
      };


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

      monoidal-containers = self.callCabal2nix "monoidal-containers" ( monoidal-containers-src ) {};

      universe-template = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "universe-template" (universe-src + /template) {});
      terminal-progress-bar = self.callHackage "terminal-progress-bar" "0.2" {};

      reflex          = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (self.callCabal2nix "reflex"          reflex-src { }));
      reflex-dom-core = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (self.callCabal2nix "reflex-dom-core" (reflex-dom-src + /reflex-dom-core) {}));
      reflex-dom      = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (self.callCabal2nix "reflex-dom"      (reflex-dom-src + /reflex-dom) { }));

      heist = pkgs.haskell.lib.doJailbreak super.heist; # allow heist to use newer version of aeson
    };
})
