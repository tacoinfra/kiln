{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.catalina.bottle.tar.gz
with pkgs;
let
    outer-version = "v9.0-rc1-1";
    tezos-accuser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-009-PsFLoren-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "0wdip4mf1bydrqi4fi2192yg6fmxdpqz6np2vrzvxvz3mc4awrd2";
    };
    tezos-accuser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-008-PtEdo2Zk-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "1f7b3s2pl3rwp2k9rsmbh4hpas1959h8cw34v71vg1inhj4zlvnq";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "0xj5917aa6f9adfi06qmizx8r7lsa30jb6il53ig6b4saj296rs0";
    };
    tezos-baker-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "0z72qp1kbgcz0vslrw523dj3k9plinfhy6skv3j8s1ia9bmdk668";
    };
    tezos-baker-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-008-PtEdo2Zk-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "1q7sq4kxd636izfb9isz1p84jp6swa328a9rfczy87fh4a98smgf";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "00l6wk5gn0jwqq3jk07w21wqk0hbn8jx401mqbgpjzg7qirm9dhp";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-codec-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "0id99lss8piynqybx3l9ci31rj6gkqdxbg31slbmrmfdvzli97a8";
    };
    tezos-endorser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "176fw0y12gqa6aksjriacyq0937nx31m4k32pjlmp5br0s0nch0k";
    };
    tezos-endorser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-008-PtEdo2Zk-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "0vhphdi2gsmnpyzdq11q8j286g6x65pdd97b3sdfimild8imjgpf";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "10an22jgs2fp2ahxgp0jilps4w2ksg8kvsi41p74pwjkv3b42xs7";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-signer-${outer-version}.catalina.bottle.tar.gz";
      sha256 = "168ysssd0f9m8qs1f8v09wylphhgzjfbwd4s548kkmihgbj3y6xy";
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
