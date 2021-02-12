{ pkgs }:

with pkgs;
let
    tezos-accuser-007-PsDELPH1 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-accuser-007-PsDELPH1-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "1ncvd82sblrrzhm8zffyliyf02ahyc0r32r3hj5q804y7dl2v7kw";
    };
    tezos-accuser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-accuser-008-PtEdo2Zk-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "17vrwxi3mvrx3nl3apz8zxghc769jpdk634fys4v2xrx9br2sn2a";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-admin-client-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "0nir4p207abf05ijqynxb170rfp4wna48gkxbdnv26sfivrr03qi";
    };
    tezos-baker-007-PsDELPH1 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-baker-007-PsDELPH1-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "06gfc8wql0k63xql6ga5r2l1vi8qq0xam9dmv0sbmfmswfp1lrsm";
    };
    tezos-baker-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-baker-008-PtEdo2Zk-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "0qj9d916dp803mm2ri2h17xlsrla7rg6jaifsh9ypqb1fyhdnnya";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-client-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "1h5r3f057f4kr0cm081k235l0a34079scmm7m8hp1zcq4lk8n814";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-codec-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "1j7c74n410br5wibvr0n6kd3hvdb8gsd65g8vbkdkzmz8adchg8d";
    };
    tezos-endorser-007-PsDELPH1 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-endorser-007-PsDELPH1-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "1b0kzx0p2y0pqgaiww7s1z5f6wv7k13a71b01vd54yiwbywd08k7";
    };
    tezos-endorser-008-PtEdo2Zk = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-endorser-008-PtEdo2Zk-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "0yhgi3r0d83q3c3c1gja0h8dfda2d6sa373bvms1fdbvym7wap8p";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-node-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "0m8cc5xd6l2j9f162b35irzd4s6lb7y3zm98agx9xjgyka0vlm7v";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.2-1/tezos-signer-v8.2-1.catalina.bottle.tar.gz";
      sha256 = "08fmyplyspcfw7ba3406x0lzswc5b6icrzrvsz7l85hsib6kj0wl";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "8.2";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-accuser-007-PsDELPH1}/v8.2-1/bin/tezos-accuser-007-PsDELPH1 $out/bin/tezos-accuser-007-PsDELPH1
  chmod +x $out/bin/tezos-accuser-007-PsDELPH1

  cp ${tezos-accuser-008-PtEdo2Zk}/v8.2-1/bin/tezos-accuser-008-PtEdo2Zk $out/bin/tezos-accuser-008-PtEdo2Zk
  chmod +x $out/bin/tezos-accuser-008-PtEdo2Zk

  cp ${tezos-admin-client}/v8.2-1/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-007-PsDELPH1}/v8.2-1/bin/tezos-baker-007-PsDELPH1 $out/bin/tezos-baker-007-PsDELPH1
  chmod +x $out/bin/tezos-baker-007-PsDELPH1

  cp ${tezos-baker-008-PtEdo2Zk}/v8.2-1/bin/tezos-baker-008-PtEdo2Zk $out/bin/tezos-baker-008-PtEdo2Zk
  chmod +x $out/bin/tezos-baker-008-PtEdo2Zk

  cp ${tezos-client}/v8.2-1/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-codec}/v8.2-1/bin/tezos-codec $out/bin/tezos-codec
  chmod +x $out/bin/tezos-codec

  cp ${tezos-endorser-007-PsDELPH1}/v8.2-1/bin/tezos-endorser-007-PsDELPH1 $out/bin/tezos-endorser-007-PsDELPH1
  chmod +x $out/bin/tezos-endorser-007-PsDELPH1

  cp ${tezos-endorser-008-PtEdo2Zk}/v8.2-1/bin/tezos-endorser-008-PtEdo2Zk $out/bin/tezos-endorser-008-PtEdo2Zk
  chmod +x $out/bin/tezos-endorser-008-PtEdo2Zk

  cp ${tezos-node}/v8.2-1/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node

  cp ${tezos-signer}/v8.2-1/bin/tezos-signer $out/bin/tezos-signer
  chmod +x $out/bin/tezos-signer
  '';
  }
