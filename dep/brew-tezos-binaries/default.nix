{ pkgs }:

with pkgs;
let
    tezos-accuser-007-PsDELPH1 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-accuser-007-PsDELPH1-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "1wc4rzlgf2z5c105x2xkrwhi4h8cjv4dgm2znkbapkig9iyxz2hy";
    };
    tezos-accuser-008-PtEdoTez = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-accuser-008-PtEdoTez-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "0x4d9zpi5bd4gaw3rax70fsi1y01h9k1mzz8akpbpmjy714d4daj";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-admin-client-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "10qr9lwxl866ra04nzzwy7kbr0qyysknqlmjlic8xfqj3sgyqav2";
    };
    tezos-baker-007-PsDELPH1 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-baker-007-PsDELPH1-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "0x9fn3hpqr3qwnzsjlmcljk82vziaxaj9i74hd7ac743f6iq0z7m";
    };
    tezos-baker-008-PtEdoTez = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-baker-008-PtEdoTez-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "04afb0gl7qacl0zjl24mrccw5y618inadqra14myfz5y1809yf63";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-client-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "1yqx51vfip624gs6mhlaslwhhpg1zrkxf0j08zs5176mlyj9p3yd";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-codec-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "1kqypyyvzvk9vv38bwknalfk7p1pcp45057mhbiaxm2yba55gw4q";
    };
    tezos-endorser-007-PsDELPH1 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-endorser-007-PsDELPH1-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "1l25pwdhiwc7v520k606aw0z0m0k6yhfg06yxr2zys0q0zn5md7m";
    };
    tezos-endorser-008-PtEdoTez = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-endorser-008-PtEdoTez-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "1hllxpbyw6ff1njbyc26px80knhzxhkgjd7mp184dfhlryrn2llb";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-node-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "0gh304xrqn37fiwnx8qf8wyzc4pdpxsgla4vjwnhsnn5m3yhrd74";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v8.1-1/tezos-signer-v8.1-1.catalina.bottle.tar.gz";
      sha256 = "0hq5yprkj9qcn2nr0r183y827294xw3cy5gy7sidkyx3653yll6c";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "8.1";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-accuser-007-PsDELPH1}/v8.1-1/bin/tezos-accuser-007-PsDELPH1 $out/bin/tezos-accuser-007-PsDELPH1
  chmod +x $out/bin/tezos-accuser-007-PsDELPH1

  cp ${tezos-accuser-008-PtEdoTez}/v8.1-1/bin/tezos-accuser-008-PtEdoTez $out/bin/tezos-accuser-008-PtEdoTez
  chmod +x $out/bin/tezos-accuser-008-PtEdoTez

  cp ${tezos-admin-client}/v8.1-1/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-007-PsDELPH1}/v8.1-1/bin/tezos-baker-007-PsDELPH1 $out/bin/tezos-baker-007-PsDELPH1
  chmod +x $out/bin/tezos-baker-007-PsDELPH1

  cp ${tezos-baker-008-PtEdoTez}/v8.1-1/bin/tezos-baker-008-PtEdoTez $out/bin/tezos-baker-008-PtEdoTez
  chmod +x $out/bin/tezos-baker-008-PtEdoTez

  cp ${tezos-client}/v8.1-1/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-codec}/v8.1-1/bin/tezos-codec $out/bin/tezos-codec
  chmod +x $out/bin/tezos-codec

  cp ${tezos-endorser-007-PsDELPH1}/v8.1-1/bin/tezos-endorser-007-PsDELPH1 $out/bin/tezos-endorser-007-PsDELPH1
  chmod +x $out/bin/tezos-endorser-007-PsDELPH1

  cp ${tezos-endorser-008-PtEdoTez}/v8.1-1/bin/tezos-endorser-008-PtEdoTez $out/bin/tezos-endorser-008-PtEdoTez
  chmod +x $out/bin/tezos-endorser-008-PtEdoTez

  cp ${tezos-node}/v8.1-1/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node

  cp ${tezos-signer}/v8.1-1/bin/tezos-signer $out/bin/tezos-signer
  chmod +x $out/bin/tezos-signer
  '';
  }
