{}: (import ./focus {}).mkDerivation {
  name = "tezos-bake-central";
  version = "0.0.1";
  commonDepends = p: with p; [
     data-default
     file-embed
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
     data-default
     resource-pool
     snap
     snap-core
     snap-loader-static
     snap-server
     obelisk-executable-config-inject
     obelisk-asset-serve-snap
  ];
}
