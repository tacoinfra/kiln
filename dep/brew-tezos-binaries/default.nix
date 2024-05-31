{ pkgs }:
with pkgs;
let
  outer-version = "20.0-1";
  macos_version = "monterey";
    tezos-baker-PtParisB = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/octez-v${outer-version}/tezos-baker-PtParisB-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0ycqs6ylcx8wz1xh6q3fdpyspjxbs3rb37k2l9dyxnyqhqbbz61s";
    };
    tezos-baker-Proxford = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/octez-v${outer-version}/tezos-baker-Proxford-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "17bkxq7zl3viy34cr0b90h7428j6ga94v25ncp3k3izaa2vblszp";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/octez-v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0j8x9sar5gh80jb5wczxzzhx4mrnn0g7a30i9gm4fr1glmhydyq9";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/octez-v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1yv6srn60225w227db771b5vn7pvc1kjb2gqwqz7l5ddpikv2faq";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtParisB}/v${outer-version}/bin/tezos-baker-PtParisB $out/bin/tezos-baker-PtParisB
  chmod +x $out/bin/tezos-baker-PtParisB

  cp ${tezos-baker-Proxford}/v${outer-version}/bin/tezos-baker-Proxford $out/bin/tezos-baker-Proxford
  chmod +x $out/bin/tezos-baker-Proxford

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
