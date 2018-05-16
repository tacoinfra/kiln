{}: 
let
  focus = import ./focus {};
  # tezos-bake-monitor-lib = p: import ../tezos-bake-monitor-lib { pkgs = p; };
in focus.mkDerivation {
  name = "tezos-bake-central";
  version = "0.0.1";
  haskellPackagesOverrides = self: super: {
    # tezos-bake-monitor-lib = self.callCabal2nix "tezos-bake-monitor-lib" ../tezos-bake-monitor-lib {};
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
  ];
}
