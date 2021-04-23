{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v9.0-1";
  macos_version = "mojave";
    tezos-accuser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0s3dlwipafwr54myxjwlr3h646zky9g2y0qf3zsqj1affrjx12ci";
    };
    tezos-accuser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0n45p8m0cnhvj8snwwv08j11x0gkmssysd6wp6hvsydi6ccy004p";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "053x0746w4mxi3jnzrkyzw3589sn8pmkbrw62vx6ph64fi6f4w84";
    };
    tezos-baker-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "150qjrrwb10n2rrgwrjkx00c8yvzr8y7zq3kdms0nkfikkikw7bg";
    };
    tezos-baker-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "17nin1sr7078bvfdljd64prx29sciw209p5vx6igxcnynrx4zshj";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0qv8d0dh14pqbhh5rwnlb2dx6mrz4p531fi7bi9h94irvafanifm";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-codec-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0c78kli6kgv6s2cq9c985fh2l1229jxrca4gcsh6vv1760ypqzs0";
    };
    tezos-endorser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "168aaxhfss7rkhpz4jnydhai7c0245fj5i7z68q1b7py1anabzzh";
    };
    tezos-endorser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-008-PtEdo2Zk-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1vcggq7zpgspmi0y8qbf69xkyff1bmdqmmmh0yyv2x1mkba7a82f";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "123pg38sbr93c9q2m3r45adb2i4f87ndvhqh86snpj3gahlswi0h";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-signer-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1z3b87lbylgdkc5p6ybcip4zx2vj7r9l2ki1gmyqn42cf756js22";
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
