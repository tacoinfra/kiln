{ pkgs }:
with pkgs;
let
  outer-version = "v17.2-1";
  macos_version = "big_sur";
    tezos-baker-PtNairob = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtNairob-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0r5am7q66x1p2qxzw718q0ln9amdiss56qp56c4wcykg2a140d6f";
    };
    tezos-baker-PtMumbai = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtMumbai-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "00g4l6fg2w5gp2rrx7shibbymy0zjz6sx6bx6h770s4y978yvm5m";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "12fwhprwgqqxz8svp0qa4n71wv3qlpkallk78vq5mdjm4xlxcd2h";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0f94x1xmbzj3vz3afwszvjda91bmxc2079m0isdvqpdw0zcg4miy";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtNairob}/${outer-version}/bin/tezos-baker-PtNairob $out/bin/tezos-baker-PtNairob
  chmod +x $out/bin/tezos-baker-PtNairob

  cp ${tezos-baker-PtMumbai}/${outer-version}/bin/tezos-baker-PtMumbai $out/bin/tezos-baker-PtMumbai
  chmod +x $out/bin/tezos-baker-PtMumbai

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
