let
  perMachine = system: let
   root = import ./. { inherit system; };
  in {

  } // root.pkgs.lib.optionalAttrs (system == "x86_64-linux") {
    dockerExe = root.pkgs.lib.hydraJob root.dockerExe;
    dockerImage = root.pkgs.lib.hydraJob root.dockerImage;
  };
in {
  x86_64-linux = perMachine "x86_64-linux";
  x86_64-darwin = perMachine "x86_64-darwin";
}
