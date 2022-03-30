{ pkgs }:
with pkgs;
let
  outer-version = "v12.1-1";
  macos_version = "catalina";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "11pycgfqcan9b42g3x0136kfy5aqj9cdcz3vc62h28p26c0rdmwy";
    };
    tezos-baker-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "15y5zf2yvam2r276q2dvr1l46k72j0ag4frfzdc3gifgsjkray5d";
    };
    tezos-baker-012-Psithaca = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-012-Psithaca-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1qhhdifzfysapf8x8zp9j20adqk5fxjpxi5smjbzdjz6rwacy99k";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1kwqq9la5z1166wl1h8z4pqvqc6gqhk1nkkq0p5w4wgcm734gk4r";
    };
    tezos-endorser-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "07vlfyihk57si0s84bbbwaam2bjwsbz0sds21w0250fwkdsyb2az";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0b3mar64d70hfi1iws6p00jd15k2i5kl3lkmf5vml5nn6y1sqryi";
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

  cp ${tezos-baker-012-Psithaca}/${outer-version}/bin/tezos-baker-012-Psithaca $out/bin/tezos-baker-012-Psithaca
  chmod +x $out/bin/tezos-baker-012-Psithaca

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-011-PtHangz2}/${outer-version}/bin/tezos-endorser-011-PtHangz2 $out/bin/tezos-endorser-011-PtHangz2
  chmod +x $out/bin/tezos-endorser-011-PtHangz2

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
