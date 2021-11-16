{ pkgs }:
with pkgs;
let
  outer-version = "v11.0-1";
  macos_version = "mojave";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1r88kl563zqjywg5v5q99jiavjrsx3nlmkj3xr75m9spx3ac07kj";
    };
    tezos-baker-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1r5wpgvf8ki5223f9dh039n5cgqn3a60s545i13pbv1ibfrs7p26";
    };
    tezos-baker-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "01yw13pd8036kp97n2wv8z1kq6i03mj7fdwkk1395r2z936v9qpi";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1i89dndk7qlcclxb84hif911zlddb8la8arswwp3v74ba4chhmsm";
    };
    tezos-endorser-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "183rmvr6723xd8q0swjyn1h4bmphla3njgfm2cwzbfniqmm76dq7";
    };
    tezos-endorser-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "01xz7z06qsfy2a8c0zq8alxqr57dh49ml32a9q8s7ik0vy5bj0wp";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1c259igmljydi1x9azph3kl55v94bwr3vjr351p6185gj6506151";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-010-PtGRANAD}/${outer-version}/bin/tezos-baker-010-PtGRANAD $out/bin/tezos-baker-010-PtGRANAD
  chmod +x $out/bin/tezos-baker-010-PtGRANAD

  cp ${tezos-baker-011-PtHangz2}/${outer-version}/bin/tezos-baker-011-PtHangz2 $out/bin/tezos-baker-011-PtHangz2
  chmod +x $out/bin/tezos-baker-011-PtHangz2

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-010-PtGRANAD}/${outer-version}/bin/tezos-endorser-010-PtGRANAD $out/bin/tezos-endorser-010-PtGRANAD
  chmod +x $out/bin/tezos-endorser-010-PtGRANAD

  cp ${tezos-endorser-011-PtHangz2}/${outer-version}/bin/tezos-endorser-011-PtHangz2 $out/bin/tezos-endorser-011-PtHangz2
  chmod +x $out/bin/tezos-endorser-011-PtHangz2

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
