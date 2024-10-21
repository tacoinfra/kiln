{ pkgs }:
with pkgs;
let
  outer-version = "20.3-1";
  macos_version = "ventura";
    tezos-baker-PtParisB = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PtParisB-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1l55qianv4vj02mg1qxfbk9hb918clnbp7wpgrlivn1ivvp43bvg";
    };
    tezos-baker-PsParisC = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsParisC-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "16zj0gq327qii5fn6485zsd50xmym2zaj5k47xsaimh5a8alxfqv";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1jga1g1y872jvyahl1p8f108mklniygbsq2g8wm3i3n9xvjbv0jj";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1pd33b0vwllvpz3csp1d0x7wjrdci3x7mwixi66w6k0v6zzfn01a";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtParisB}/v${outer-version}/bin/tezos-baker-PtParisB $out/bin/tezos-baker-PtParisB
  chmod +x $out/bin/tezos-baker-PtParisB

  cp ${tezos-baker-PsParisC}/v${outer-version}/bin/tezos-baker-PsParisC $out/bin/tezos-baker-PsParisC
  chmod +x $out/bin/tezos-baker-PsParisC

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
