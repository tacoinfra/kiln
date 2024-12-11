{ pkgs }:
with pkgs;
let
  outer-version = "21.1-1";
  macos_version = "ventura";
    tezos-baker-PsQuebec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsQuebec-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1i8w44kwbybixdawvasq8mbg787vkl6g7yp9q1yj8qw15n7q7dh4";
    };
    tezos-baker-PsParisC = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsParisC-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0292ds4qx6rgagfss41jijr7vc1qr9k3zjvsgdn027qkb2d1dddm";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1hsd54lq8y32s1j48p0an6r1mp56mjna6gcvzp05ynk9rwwmwfqm";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "06rbcrad0b9dl03805ibcsl3bd1gcwn4bajyzybj4i041mhvfyay";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PsQuebec}/v${outer-version}/bin/tezos-baker-PsQuebec $out/bin/tezos-baker-PsQuebec
  chmod +x $out/bin/tezos-baker-PsQuebec

  cp ${tezos-baker-PsParisC}/v${outer-version}/bin/tezos-baker-PsParisC $out/bin/tezos-baker-PsParisC
  chmod +x $out/bin/tezos-baker-PsParisC

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
