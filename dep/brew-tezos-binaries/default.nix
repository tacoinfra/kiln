{ pkgs }:
with pkgs;
let
  outer-version = "20.2-1";
  macos_version = "monterey";
    tezos-baker-PtParisB = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PtParisB-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "01b71jv4cpcc4c19rqvkhxbg79c963kk3bigv72n7l1cwpha9g84";
    };
    tezos-baker-PsParisC = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsParisC-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "18q2jvp8njpsgayzk648maiv4xh9bnfvi37vfx9155fqgy0q3f3n";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1jzh96a1yfn7wn7hwjv5mj69sbjn4mx9c4xjjx9nzh9ypixcc0fx";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1bb65ffy0cxnhgkf897iwgh82ssl2nmswk39raz259kg5fpfizwz";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtParisB}/v${outer-version}/bin/tezos-baker-PtParisB $out/bin/tezos-baker-PtParisB
  chmod +x $out/bin/tezos-baker-PtParisB

  cp ${tezos-baker-PsParisC}/v${outer-version}/bin/tezos-baker-PsParisC $out/bin/tezos-baker-PsParisC
  chmod +x $out/bin/tezos-baker-PsParisC

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
