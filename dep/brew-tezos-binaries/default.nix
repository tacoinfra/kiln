{ pkgs }:
with pkgs;
let
  outer-version = "v11.1-1";
  macos_version = "catalina";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1bmihyfcbai86vsdniqbakdlmll1rkq33ll8d6pnjqyyp50mxwgl";
    };
    tezos-baker-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1ijzq9jh4jax9yalj66pgmdah3zv8iaia56hrjqnyrq498pvy8lp";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0g62pszyxcjb97p1svkgypgl6qvwqwwg82chp414ndq69f8xi8rg";
    };
    tezos-endorser-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "046xcbzr0if45m6z8i3dlpr3f66jf2jrrwhmkkgykg2fwad0pvzn";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0nn225qpnawfw62z8470zv8vzdbkvdhkjdgczwrv83ld1jxjqxjx";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-011-PtHangz2}/${outer-version}/bin/tezos-baker-011-PtHangz2 $out/bin/tezos-baker-011-PtHangz2
  chmod +x $out/bin/tezos-baker-011-PtHangz2

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-011-PtHangz2}/${outer-version}/bin/tezos-endorser-011-PtHangz2 $out/bin/tezos-endorser-011-PtHangz2
  chmod +x $out/bin/tezos-endorser-011-PtHangz2

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
