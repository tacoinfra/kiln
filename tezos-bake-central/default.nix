let
  obelisk = import .obelisk/impl { system = builtins.currentSystem; iosSdkVersion = "10.2"; };
in
obelisk.project ./. ({ pkgs, ... }: let
    reflex-platform = obelisk.reflex-platform;

    rhyolite-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "rhyolite";
      rev = "487900195115f399ec8f1825510ea22c08bc8f3b";
      sha256 = "1fr2xgw512xkr4h4fazqh7hldgjw3hq6y8x5qadzijryk449bky8";
      private = true;
    };

    gargoyle-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "80dfffb22aa399a08559db4191d6d9da8569386d";
      sha256 = "17hhfm20k1d3p1alxgs7dm3nayivr362w3al38mz9v6rab3ywzjc";
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

      # rhyolite-backend needs a custom dependency injection
      rhyolite-backend-snap = rhyolite-src + /backend-snap;
      rhyolite-common = rhyolite-src + /common;
      rhyolite-frontend = rhyolite-src + /frontend;
    };
    overrides = self: super: {
      # tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {};

      gargoyle-postgresql-nix = pkgs.haskell.lib.addBuildTools
        (self.callCabal2nix "gargoyle-postgresql-nix" (gargoyle-src + /gargoyle-postgresql-nix) {})
        [ pkgs.postgresql ]; # TH use of `staticWhich` for `psql` requires this on the PATH during build time.

      rhyolite-backend = self.callCabal2nix "rhyolite-backend" (rhyolite-src + /backend) { websockets = self.websockets-obsidian; };

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

      # Needed?
      heist = pkgs.haskell.lib.doJailbreak super.heist; # allow heist to use newer version of aeson
    };
})
