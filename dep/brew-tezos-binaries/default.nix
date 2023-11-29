{ pkgs }:
with pkgs;
let
  outer-version = "v18.1-1";
  macos_version = "big_sur";
    tezos-baker-PtNairob = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtNairob-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "00q6biz5bdn3vqc64964hzs32zcprz7ar99hs20fmgvwzmn2n21j";
    };
    tezos-baker-Proxford = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-Proxford-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1z20i8sswnl7sykbg0jv0r5p09r1fxrck9qgrwmp7604yzbqzn1f";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "033yf48hxcm2cfr9pw4z3r6h0kpkhls559x6q6pswb06rs0hg1ra";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0xb31fsl37mw0sm3qm3hdyvny1dpwi2922yr3wa95z6y8rad31as";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtNairob}/${outer-version}/bin/tezos-baker-PtNairob $out/bin/tezos-baker-PtNairob
  chmod +x $out/bin/tezos-baker-PtNairob

  cp ${tezos-baker-Proxford}/${outer-version}/bin/tezos-baker-Proxford $out/bin/tezos-baker-Proxford
  chmod +x $out/bin/tezos-baker-Proxford

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
