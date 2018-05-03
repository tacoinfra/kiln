{}: 
let
  focus = import ./focus {};
  # tezos-bake-monitor-lib = p: import ../tezos-bake-monitor-lib { pkgs = p; };
in focus.mkDerivation {
  name = "tezos-bake-central";
  version = "0.0.1";
  haskellPackagesOverrides = self: super: {
    tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {};
  };
  commonDepends = p: with p; [
    either
    data-default
    file-embed
    tezos-bake-monitor-lib
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
  ];
  backendDepends = p: with p; [
    stm
    mime-mail
    safe
    monad-logger
    data-default
    resource-pool
    monad-control
    groundhog
    groundhog-postgresql
    http-conduit
    postgresql-simple
    clientsession
    snap
    snap-core
    snap-loader-static
    snap-server
    obelisk-executable-config-inject
    obelisk-asset-serve-snap
  ];
}
