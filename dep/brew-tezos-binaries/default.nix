{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v8.3-1";
  macos_version = "mojave";
    # tezos-accuser-009-PsFLoren = fetchTarball {
    #   url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
    #   sha256 = "0wdip4mf1bydrqi4fi2192yg6fmxdpqz6np2vrzvxvz3mc4awrd2";
    # };
    tezos-accuser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0i55hryfr1ycj960km4m5m8s29dhfn23gy8vj29lyf812yly02ll";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1dyhrw6jha0yws3sdsv363myz5qsmxs84bmwn3b552lprfssa57k";
    };
    # tezos-baker-009-PsFLoren = fetchTarball {
    #   url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
    #   sha256 = "0z72qp1kbgcz0vslrw523dj3k9plinfhy6skv3j8s1ia9bmdk668";
    # };
    tezos-baker-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "04yma1bfqqz5hx21dmdm71c3dd79wbb03v5mbwmdd1xn0pp2dg4v";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0v3l11bjwc2cvwzhzfa9mh2ma83xxz480508xs275bfm2551jai3";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-codec-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "16iwxabwwkm0fxicpfcianmacfjgs1lg91mif0qqzyxgywvv7v9c";
    };
    # tezos-endorser-009-PsFLoren = fetchTarball {
    #   url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
    #   sha256 = "176fw0y12gqa6aksjriacyq0937nx31m4k32pjlmp5br0s0nch0k";
    # };
    tezos-endorser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "11bs9zan50z3jz9dxnlpj9r587qxi0x99ijpsby1pj2h783dcy65";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0k2066ckfpzwg6qmdjhqbczh0vhr91bc60hwhlgpbj70z480ms3c";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-signer-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0n5jaivmzai20149n0y4fdwyn97j54pv3mjxrhas0sp7gsssvvpc";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  # installPhase = ''
  # mkdir -p $out/bin

  # cp ${tezos-accuser-009-PsFLoren}/${outer-version}/bin/tezos-accuser-009-PsFLoren $out/bin/tezos-accuser-009-PsFLoren
  # chmod +x $out/bin/tezos-accuser-009-PsFLoren

  # cp ${tezos-accuser-008-PtEdo2Zk}/${outer-version}/bin/tezos-accuser-008-PtEdo2Zk $out/bin/tezos-accuser-008-PtEdo2Zk
  # chmod +x $out/bin/tezos-accuser-008-PtEdo2Zk

  # cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  # chmod +x $out/bin/tezos-admin-client

  # cp ${tezos-baker-009-PsFLoren}/${outer-version}/bin/tezos-baker-009-PsFLoren $out/bin/tezos-baker-009-PsFLoren
  # chmod +x $out/bin/tezos-baker-009-PsFLoren

  # cp ${tezos-baker-008-PtEdo2Zk}/${outer-version}/bin/tezos-baker-008-PtEdo2Zk $out/bin/tezos-baker-008-PtEdo2Zk
  # chmod +x $out/bin/tezos-baker-008-PtEdo2Zk

  # cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  # chmod +x $out/bin/tezos-client

  # cp ${tezos-codec}/${outer-version}/bin/tezos-codec $out/bin/tezos-codec
  # chmod +x $out/bin/tezos-codec

  # cp ${tezos-endorser-009-PsFLoren}/${outer-version}/bin/tezos-endorser-009-PsFLoren $out/bin/tezos-endorser-009-PsFLoren
  # chmod +x $out/bin/tezos-endorser-009-PsFLoren

  # cp ${tezos-endorser-008-PtEdo2Zk}/${outer-version}/bin/tezos-endorser-008-PtEdo2Zk $out/bin/tezos-endorser-008-PtEdo2Zk
  # chmod +x $out/bin/tezos-endorser-008-PtEdo2Zk

  # cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  # chmod +x $out/bin/tezos-node

  # cp ${tezos-signer}/${outer-version}/bin/tezos-signer $out/bin/tezos-signer
  # chmod +x $out/bin/tezos-signer
  # '';

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-accuser-008-PtEdo2Zk}/${outer-version}/bin/tezos-accuser-008-PtEdo2Zk $out/bin/tezos-accuser-008-PtEdo2Zk
  chmod +x $out/bin/tezos-accuser-008-PtEdo2Zk

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-008-PtEdo2Zk}/${outer-version}/bin/tezos-baker-008-PtEdo2Zk $out/bin/tezos-baker-008-PtEdo2Zk
  chmod +x $out/bin/tezos-baker-008-PtEdo2Zk

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-codec}/${outer-version}/bin/tezos-codec $out/bin/tezos-codec
  chmod +x $out/bin/tezos-codec

  cp ${tezos-endorser-008-PtEdo2Zk}/${outer-version}/bin/tezos-endorser-008-PtEdo2Zk $out/bin/tezos-endorser-008-PtEdo2Zk
  chmod +x $out/bin/tezos-endorser-008-PtEdo2Zk

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node

  cp ${tezos-signer}/${outer-version}/bin/tezos-signer $out/bin/tezos-signer
  chmod +x $out/bin/tezos-signer
  '';

  }
