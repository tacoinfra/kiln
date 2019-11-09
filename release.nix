{ local-self ? import ./. {}
}:

let
  inherit (local-self.pkgs) lib runCommand nix;

  cacheBuildSystems =
    [ "x86_64-linux"
      "x86_64-darwin"
    ];
  perPlatform = lib.genAttrs cacheBuildSystems (system: let
    root = import ./. { inherit system; };
  in {
    ghc = {
      inherit (root.ghc) frontend backend common;
    };
    ghcjs = {
      inherit (root.ghcjs) frontend common;
    };
  } // root.pkgs.lib.optionalAttrs (system == "x86_64-linux") {
    inherit (root) dockerExe dockerImage kilnVM kilnVMSystem kiln-debian exe;
  } // root.pkgs.lib.optionalAttrs (system == "x86_64-darwin") {
    inherit (root) exe;
  });

# Mash these into a single level for ci. Without this, just returning perPlatform
# was actually building nothing because none of the attr values are derivations.
# We  need a flat set of derivations to properly build everything in CI.
# I'm hoping that there is a nicer way to collect this together because this is kinda manky. Lol.
in lib.fold
  (system: o: o // lib.mapAttrs' (n: v: lib.nameValuePair "${system}_${n}" v) (lib.getAttr system perPlatform))
  {}
  (lib.attrNames perPlatform)
