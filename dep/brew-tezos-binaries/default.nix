{ pkgs }:
with pkgs;
let
  outer-version = "v14.0-1";
  macos_version = "big_sur";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1qlcxb20v2nbycd8z95bp7ybd4qqh5ik5wfw2anrd87jy7f7mnbn";
    };
    tezos-baker-013-PtJakart = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-013-PtJakart-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0v0mw9lag75qb8kfwdz0kmq8y5dq4fxpljlidkjzi4a6f718djzp";
    };
    tezos-baker-014-PtKathma = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-014-PtKathma-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1cpwqk3h5m4spxfgrijr6xk8jiqy821hnp63plqixzfrgy5a8d15";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1shqlwv2gwki04h0k5nbfqiblga7zm31ixpgyw44cljaw542shid";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1ss9hmha4i7n2zgsy65d5x34vj9fz61awa8x3ng3klprzbkxaf11";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-013-PtJakart}/${outer-version}/bin/tezos-baker-013-PtJakart $out/bin/tezos-baker-013-PtJakart
  chmod +x $out/bin/tezos-baker-013-PtJakart

  cp ${tezos-baker-014-PtKathma}/${outer-version}/bin/tezos-baker-014-PtKathma $out/bin/tezos-baker-014-PtKathma
  chmod +x $out/bin/tezos-baker-014-PtKathma

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
