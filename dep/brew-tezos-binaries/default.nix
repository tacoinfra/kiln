{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v9.1-1";
  macos_version = "mojave";
    tezos-accuser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1wqhpj9rwja7pcdmrdcl2jjfqfxj43zcxjypmnsgjw5203k9v108";
    };
    tezos-accuser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0vgvqc71xcxhv2xrpj3lhis6wvhykhz2s43n8iwmyhhxp9sphsz8";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "13w2ngrb37fnvi155wcdbjwsl8nq6mz7amqslp4wm9kqynfrwgfz";
    };
    tezos-baker-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1d5s1k9rgn1rizc8x1lzc2c2n7h0xqx6pxiy2qgc27swxn91s2m5";
    };
    tezos-baker-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1gxqqhasrki08rnbhs1ykhxh7rliskc6pk56lik6x6fq3n762ikq";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "03gs7lypm3ig5sl900yhqd89vb12s8c0ffgdm3cpcwvxmlf43709";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-codec-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0ml550ya5y6gd2b0k755dj7rfr3xy1z1fyz9f8880n7nyb7zhs27";
    };
    tezos-endorser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "04cib5w28li73v6zww2h7ys5qa8c7qjn6g0cbsh6f52hwvy09mzh";
    };
    tezos-endorser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "16pmpbdahpij38cmxc244ngi2shx8dlnrpkn0blr6ivzsm3gr286";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0arvx4nygj8m9namqnn9wzd47j860rid7xdhhx72s22ns527gk6n";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-signer-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1hcalzsfw6ql3xs54v3y99h4c9vyy5bc2b33j4dkm3y12xj8jmiv";
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
