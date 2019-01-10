let root = import ./. {}; in
{
  dockerExe = root.pkgs.lib.hydraJob root.dockerExe;
  dockerImage = root.pkgs.lib.hydraJob root.dockerImage;
}
