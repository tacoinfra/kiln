{ pkgs }:
with pkgs;
let
  outer-version = "21.2-1";
  macos_version = "ventura";
    tezos-baker-PsQuebec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsQuebec-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1346qvab9nwq7ps21bzpqhgqf12ixmzxd1pqdh2pdwzjmdp2zk90";
    };
    tezos-baker-PsParisC = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsParisC-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "07dhm7bd6fi77dfww6n91cgsn1p9k6a3vgi9zxic6iqmy5wz69rl";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1wxwb8vfssb9gcm5i1z4xxgcmcjm5p3iqpaddf3r972wq1bvfw38";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "12y9aim72nydrkgjgwxr1r432isr4f2dv0shr56x2azyjb1fffjy";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PsQuebec}/v${outer-version}/bin/tezos-baker-PsQuebec $out/bin/tezos-baker-PsQuebec
  chmod +x $out/bin/tezos-baker-PsQuebec

  cp ${tezos-baker-PsParisC}/v${outer-version}/bin/tezos-baker-PsParisC $out/bin/tezos-baker-PsParisC
  chmod +x $out/bin/tezos-baker-PsParisC

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
