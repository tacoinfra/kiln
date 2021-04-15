{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v9.0-rc2-1";
  macos_version = "mojave";
    tezos-accuser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1griwfrzb92hkpygg1anpqsi2gvw1vdlg4a48mqdjh6c54bj6kf3";
    };
    tezos-accuser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0xrfsig96r07zg4jbqdjr3lc1w1av5acvhs5shx0mg9w4k1c44ws";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1hg26zfn723x8mv60s8slprbzg6n5vwxq08bl18wpg9kb9823mhh";
    };
    tezos-baker-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0jjsf1sc668qfqkmnr9b0479g0l414x7j9rpj7fw3k7adk47s7qa";
    };
    tezos-baker-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0gka06xg2fa8hxhhln0w056nid43br79426afkcb7jb5mb3g6ykg";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1sg5m0cf0pb56355cnrkdw7ai28vvm2c393ssmbdh98j437ly4zk";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-codec-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1h6znxh1z3lcsxr327ybc5gc06s6fdpz3k4gjpd18axgrnjjvms2";
    };
    tezos-endorser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "06spyd07qsjrmldkgkvs8vs4xvsic57jr09b2g9q8hhhxz9arhid";
    };
    tezos-endorser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "09lbnx3hh9ira5y9avzag1dbdy37bq6i5ab0lvlib06vfpk9k1sl";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "07yvyhddw628p70j3b9rcxawh1ymlxzf4y0m2gwazfby8ds2dz65";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-signer-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0d1qgkbrc6wxj3nww5lilxmm7fwnchnxnh1vps9vpkb6vggq0kzm";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-accuser-009-PsFLoren}/${outer-version}/bin/tezos-accuser-009-PsFLoren $out/bin/tezos-accuser-009-PsFLoren
  chmod +x $out/bin/tezos-accuser-009-PsFLoren

  cp ${tezos-accuser-008-PtEdo2Zk}/${outer-version}/bin/tezos-accuser-008-PtEdo2Zk $out/bin/tezos-accuser-008-PtEdo2Zk
  chmod +x $out/bin/tezos-accuser-008-PtEdo2Zk

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-009-PsFLoren}/${outer-version}/bin/tezos-baker-009-PsFLoren $out/bin/tezos-baker-009-PsFLoren
  chmod +x $out/bin/tezos-baker-009-PsFLoren

  cp ${tezos-baker-008-PtEdo2Zk}/${outer-version}/bin/tezos-baker-008-PtEdo2Zk $out/bin/tezos-baker-008-PtEdo2Zk
  chmod +x $out/bin/tezos-baker-008-PtEdo2Zk

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-codec}/${outer-version}/bin/tezos-codec $out/bin/tezos-codec
  chmod +x $out/bin/tezos-codec

  cp ${tezos-endorser-009-PsFLoren}/${outer-version}/bin/tezos-endorser-009-PsFLoren $out/bin/tezos-endorser-009-PsFLoren
  chmod +x $out/bin/tezos-endorser-009-PsFLoren

  cp ${tezos-endorser-008-PtEdo2Zk}/${outer-version}/bin/tezos-endorser-008-PtEdo2Zk $out/bin/tezos-endorser-008-PtEdo2Zk
  chmod +x $out/bin/tezos-endorser-008-PtEdo2Zk

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node

  cp ${tezos-signer}/${outer-version}/bin/tezos-signer $out/bin/tezos-signer
  chmod +x $out/bin/tezos-signer
  '';


  }
