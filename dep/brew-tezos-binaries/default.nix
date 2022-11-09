{ pkgs }:
with pkgs;
let
  outer-version = "v15.0-1";
  macos_version = "big_sur";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "08j0pklgwzhgni9l6yajyf83hsd4dw0nmyyjsbz91i5xsjybs9yp";
    };
    tezos-baker-PtLimaPt = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtLimaPt-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "09svciqsk7x0rrnlhvscg1flaccjlkj3mim1mymnf4w6l33wqgpk";
    };
    tezos-baker-PtKathma = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtKathma-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1lqd5qzvyq8zpfyzf4k5pmvws5ln5a9wjjsd6v2j5yp3g9bg2in6";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1ghdf4cv8x53lszl099wivv9by9gksifhi9yfxb6vrvfapj22lpp";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0jb3rj3pifqypyc0ynnw1nad4dn1hm8mq0fwnjblgazgg427mx6c";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-PtLimaPt}/${outer-version}/bin/tezos-baker-PtLimaPt $out/bin/tezos-baker-PtLimaPt
  chmod +x $out/bin/tezos-baker-PtLimaPt

  cp ${tezos-baker-PtKathma}/${outer-version}/bin/tezos-baker-PtKathma $out/bin/tezos-baker-PtKathma
  chmod +x $out/bin/tezos-baker-PtKathma

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
