{ local-self ? import ./. {}
}:

let
  inherit (local-self.pkgs) lib runCommand nix;

  cacheBuildSystems =
    [ "x86_64-linux"
      # "x86_64-darwin"
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
    dockerExe = root.dockerExe;
    dockerImage = root.dockerImage;
    kilnVM = root.kilnVM;
    kiln-debian = root.kiln-debian;
    appAll = root.all;
  });

in perPlatform.x86_64-linux
