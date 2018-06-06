{ reflex-platform ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "1a437cb41476f37bae029104a1d7585235c24275";
    sha256 = "1k0l2w16g7b0srghpi90z6idbgc0a16k90q3zb8601hvnrsaw5a5";
  }) {}
}:
let
  focus = import ./focus {};

  rhyolite-src = /home/elliot/obsidian/rhyolite;
  gargoyle-src = reflex-platform.nixpkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "gargoyle";
    rev = "80dfffb22aa399a08559db4191d6d9da8569386d";
    sha256 = "17hhfm20k1d3p1alxgs7dm3nayivr362w3al38mz9v6rab3ywzjc";
  };
  groundhog-src = reflex-platform.nixpkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "groundhog";
    rev = "c2f18be45e3233f6268c8468eb0732dd6b2e8009";
    sha256 = "1r9i78bsnm6idbvp87gjklnr10g7c83nsbnrffkyrn1wmd7zzqdn";
  };

  # tezos-bake-monitor-lib = p: import ../tezos-bake-monitor-lib { pkgs = p; };
in focus.mkDerivation {
  name = "tezos-bake-central";
  version = "0.0.1";
  haskellPackagesOverrides = self: super: {
    # tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {};

    gargoyle-postgresql-nix = reflex-platform.nixpkgs.haskell.lib.addBuildTools
      (self.callCabal2nix "gargoyle-postgresql-nix" (gargoyle-src + /gargoyle-postgresql-nix) {})
      [ reflex-platform.nixpkgs.postgresql ]; # TH use of `staticWhich` for `psql` requires this on the PATH during build time.

    groundhog = self.callCabal2nix "groundhog" (groundhog-src + /groundhog) {};
    groundhog-postgresql = self.callCabal2nix "groundhog-postgresql" (groundhog-src + /groundhog-postgresql) {};
    groundhog-th = self.callCabal2nix "groundhog-th" (groundhog-src + /groundhog-th) {};

    rhyolite-backend = self.callCabal2nix "rhyolite-backend" (rhyolite-src + /backend) {};
    rhyolite-backend-snap = self.callCabal2nix "rhyolite-backend-snap" (rhyolite-src + /backend-snap) {};
    rhyolite-common = self.callCabal2nix "rhyolite-common" (rhyolite-src + /common) {};
    rhyolite-frontend = self.callCabal2nix "rhyolite-frontend" (rhyolite-src + /frontend) {};
    rhyolite-frontend-run = self.callCabal2nix "rhyolite-frontend-run" (rhyolite-src + /frontend-run) {};
  };
  commonDepends = p: with p; [
    either
    data-default
    file-embed
    # tezos-bake-monitor-lib
    cases
    scientific
    base16-bytestring
    attoparsec
    rhyolite-common
  ];
  frontendDepends = p: with p; [
    data-default
    file-embed
    focus-js
    ghcjs-dom
    reflex
    reflex-dom
    these
    obelisk-executable-config
    rhyolite-frontend
    #rhyolite-frontend-run
  ];
  backendDepends = p: with p; [
    clientsession
    data-default
    groundhog
    groundhog-postgresql
    http-client
    http-client-tls
    http-conduit
    http-types
    mime-mail
    monad-control
    monad-logger
    network
    obelisk-asset-serve-snap
    obelisk-executable-config-inject
    postgresql-simple
    resource-pool
    safe
    snap
    snap-core
    snap-loader-static
    snap-server
    stm
    diagrams-svg
    Chart-diagrams
    colour
    Chart
    diagrams-core
    diagrams-lib
    svg-builder
    lens-aeson
    rhyolite-backend
    rhyolite-backend-snap
  ];
}
