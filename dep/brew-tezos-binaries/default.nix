{ pkgs }:
with pkgs;
let
  outer-version = "v20.0-rc1-1";
  macos_version = "monterey";
    tezos-baker-PtParisB = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtParisB-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1f7ppgf724y2rhcsbyq510ybgrwn2ajgi148g0vdldrh2ixdfidp";
    };
    tezos-baker-Proxford = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-Proxford-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "125d3wa35a07mp66hpaxk57jwqrx9npmynr25qdcn8fwdld8i0x0";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0nva6zvhr7hnmiq6v8pc9mh685zm96cin4nzcaphsziwp84jadmj";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "050kdk51m2wqnziyg1q8aq0f4q3zlaa047m8w3qjkz18n2c8y49p";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtParisB}/${outer-version}/bin/tezos-baker-PtParisB $out/bin/tezos-baker-PtParisB
  chmod +x $out/bin/tezos-baker-PtParisB

  cp ${tezos-baker-Proxford}/${outer-version}/bin/tezos-baker-Proxford $out/bin/tezos-baker-Proxford
  chmod +x $out/bin/tezos-baker-Proxford

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
